{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

-- |
-- Module      : Plugin.CurryPlugin.Monad
-- Description : Convenience wrapper for the effect
-- Copyright   : (c) Kai-Oliver Prott (2020)
-- Maintainer  : kai.prott@hotmail.de
--
-- This module contains the actual monad used by the plugin and a few
-- convenicence functions.
-- The monad type is a wrapper over the
-- 'Lazy' type from 'Plugin.Effect.CurryEffect'.
module Plugin.CurryPlugin.Monad
  ( Tree (..),
    type (-->) (..),
    (?),
    failed,
    share,
    SearchMode (..),
    Normalform (..),
    modeOp,
    allValues,
    allValuesNF,
    NondetTag (..),
    liftTree1,
    liftTree2,
    app,
    apply2,
    apply2Unlifted,
    apply3,
    bind,
    rtrn,
    rtrnFunc,
    fmp,
    shre,
    shreTopLevel,
    seqValue,
    rtrnFuncUnsafePoly,
    appUnsafePoly,
  )
where

-- import Plugin.CurryPlugin.Tree

-- imports from Tree module

import Control.Applicative
import Control.Monad
import GHC.Exts
import Language.Haskell.TH.Syntax hiding (Q)
import Plugin.Effect.Annotation
import Plugin.Effect.Classes
import Plugin.Effect.Transformers
import Unsafe.Coerce

-- | Nondeterministic can be represented as trees, where results are
-- annotated at the leaves and nodes correspond to choices.
data Tree a
  = Failed
  | Leaf a
  | Choice (Tree a) (Tree a)
  deriving (Show, Functor)

instance Applicative Tree where
  pure a = build' (\_ leaf _ -> leaf a)
  {-# INLINE pure #-}
  Failed <*> _ = build' (\failed' _ _ -> failed')
  Leaf f <*> t = fmap f t
  Choice tl tr <*> t = build' (\failed' leaf choice -> choice (fold failed' leaf choice (tl <*> t)) (fold failed' leaf choice (tr <*> t)))

instance Alternative Tree where
  empty = build' (\failed' _ _ -> failed')
  tl <|> tr = build' (\failed' leaf choice -> choice (fold failed' leaf choice tl) (fold failed' leaf choice tr))

build' :: forall a. (forall b. b -> (a -> b) -> (b -> b -> b) -> b) -> Tree a
build' g = g Failed Leaf Choice
{-# NOINLINE [1] build' #-}

fold :: b -> (a -> b) -> (b -> b -> b) -> Tree a -> b
fold failed' _ _ Failed = failed'
fold _ leaf _ (Leaf a) = leaf a
fold failed' leaf choice (Choice leftTree rightTree) =
  choice
    (fold failed' leaf choice leftTree)
    (fold failed' leaf choice rightTree)
{-# NOINLINE [1] fold #-}

instance Monad Tree where
  tree >>= f =
    build'
      ( \failed' leaf choice ->
          fold
            failed'
            (fold failed' leaf choice . f)
            choice
            tree
      )
  {-# INLINE (>>=) #-}

{-# RULES
"treeFold/treeBuild" forall
  failed'
  leaf
  choice
  (g :: forall b. b -> (a -> b) -> (b -> b -> b) -> b).
  fold failed' leaf choice (build' g) =
    g failed' leaf choice
  #-}

instance MonadFail Tree where
  fail _ = build' (\failed _ _ -> failed)

instance MonadPlus Tree where
  mzero = build' (\failed _ _ -> failed)
  mplus arg1 arg2 = build' (\failed leaf choice -> choice (fold failed leaf choice arg1) (fold failed leaf choice arg2))

-- * Search algorithms

-- | Depth-first traversal of a choice tree to collect results into a list.
dfs :: Tree a -> [a]
dfs t = dfs' t []
  where
    dfs' (Leaf a) = (a :)
    dfs' (Choice t1 t2) = dfs' t1 . dfs' t2
    dfs' Failed = id

-- | Breadth-first traversal of a choice tree to collect results into a list.
bfs :: Tree a -> [a]
bfs t = bfs' [t]
  where
    bfs' (Leaf a :< q) = a : bfs' q
    bfs' (Choice t1 t2 :< q) = bfs' (t2 :< t1 :< q)
    bfs' (Failed :< q) = bfs' q
    bfs' Nil = []

---------------------------------------
-- Queue Implementation
---------------------------------------

data Queue a = Q [a] [a]
  deriving (Show, Eq, Ord, Functor)

{-# COMPLETE (:<), Nil #-}

infixr 5 :<

pattern (:<) :: a -> Queue a -> Queue a
pattern x :< xs <-
  (uncons -> Just (x, xs))
  where
    x :< xs = enqueue x xs

pattern Nil :: Queue a
pattern Nil <-
  (uncons -> Nothing)
  where
    Nil = emptyQueue

instance IsList (Queue a) where
  type Item (Queue a) = a
  fromList = flip Q []
  toList (Q xs ys) = xs ++ reverse ys

instance Semigroup (Queue a) where
  Q [] _ <> q = q
  Q xs1 ys1 <> Q xs2 ys2 = Q xs1 (ys1 ++ reverse xs2 ++ ys2)

instance Monoid (Queue a) where
  mempty = emptyQueue

emptyQueue :: Queue a
emptyQueue = Q [] []

enqueue :: a -> Queue a -> Queue a
enqueue x (Q xs ys) = queue xs (x : ys)

uncons :: Queue a -> Maybe (a, Queue a)
uncons q = (,) <$> peek q <*> dequeue q

peek :: Queue a -> Maybe a
peek (Q (x : _) _) = Just x
peek _ = Nothing

dequeue :: Queue a -> Maybe (Queue a)
dequeue (Q (_ : xs) ys) = Just (queue xs ys)
dequeue _ = Nothing

-- Invariant: If the first list is empty, then also the second list is empty.
queue :: [a] -> [a] -> Queue a
queue [] ys = Q (reverse ys) []
queue xs ys = Q xs ys

-- Here starts the original Nondet module

-- | The actual monad for nondeterminism used by the plugin.
-- newtype Nondet a = Nondet { unNondet :: LazyT Nondet Tree a }
--   deriving (Functor, Applicative, Monad, Alternative, MonadPlus, Sharing)
--     via LazyT Nondet Tree
--   deriving anyclass (SharingTop)
instance Sharing Tree where
  share = return

instance SharingTop Tree where
  shareTopLevel _ = id

{-# INLINE bind #-}
bind :: Tree a -> (a -> Tree b) -> Tree b
bind = (>>=)

{-# INLINE rtrn #-}
rtrn :: a -> Tree a
rtrn = pure

{-# INLINE rtrnFunc #-}
rtrnFunc :: (Tree a -> Tree b) -> Tree (a --> b)
rtrnFunc = pure

{-# INLINE app #-}
app :: Tree (a --> b) -> Tree a -> Tree b
app mf ma = mf >>= \f -> f ma

-- HACK:
-- RankNTypes are not really supported for various reasons,
-- but to test rewrite rules, we needed them to be supported at least
-- when the functions with RankN types are used and defined in the same module.
-- However, imagine we have a lambda with a (rank 2) type
-- (forall x. blah) -> blub.
-- Its lifted variant is something like
-- (forall x. blah') --> blub'
-- If we "unpack" the (-->) type constructor we get
-- m (forall x. blah') -> m blub'
-- This is bad, because the lifted type of the argument (forall x. blah)
-- is (forall x. m blah') and not m (forall x. blah').
-- To remedy this, we provide the following two functions using unsafeCoerce to
-- accomodate such a RankN type.
{-# INLINE rtrnFuncUnsafePoly #-}
rtrnFuncUnsafePoly :: forall a b a'. (a' -> Tree b) -> Tree (a --> b)
rtrnFuncUnsafePoly f = pure (unsafeCoerce f :: Tree a -> Tree b)

{-# INLINE appUnsafePoly #-}
appUnsafePoly :: forall a b a'. Tree (a --> b) -> a' -> Tree b
appUnsafePoly mf ma = mf >>= \f -> (unsafeCoerce f :: a' -> Tree b) ma

{-# INLINE fmp #-}
fmp :: (a -> b) -> Tree a -> Tree b
fmp = fmap

{-# INLINE shre #-}
shre :: Shareable Tree a => Tree a -> Tree (Tree a)
shre = share

{-# INLINE [0] shreTopLevel #-}
shreTopLevel :: (Int, String) -> Tree a -> Tree a
shreTopLevel = shareTopLevel

{-# INLINE seqValue #-}
seqValue :: Tree a -> Tree b -> Tree b
seqValue a b = a >>= \a' -> a' `seq` b

{-# RULES
"bind/rtrn" forall f x. bind (rtrn x) f = f x
"shreTopLevel" forall x i. shreTopLevel i x = x
  #-}

-- "bind/rtrn'let"   forall e x. let b = e in rtrn x = rtrn (let b = e in x)

-- | Treeerministic failure
failed :: Shareable Tree a => Tree a
failed = mzero

infixr 0 ?

{-# INLINE (?) #-}

-- | Treeerministic choice
(?) :: Shareable Tree a => Tree (a --> a --> a)
(?) = rtrnFunc $ \t1 -> rtrnFunc $ \t2 -> t1 `mplus` t2

-- | Enumeration of available search modes.
data SearchMode
  = -- | depth-first search
    DFS
  | -- | breadth-first search
    BFS
  deriving (Lift)

-- | Function to map the search type to the function implementing it.
modeOp :: SearchMode -> Tree a -> [a]
modeOp DFS = dfs
modeOp BFS = bfs

-- | Collect the results of a nondeterministic computation
-- as their normal form in a tree.
allValuesNF ::
  Normalform Tree a b =>
  Tree a ->
  Tree b
allValuesNF = allValues . nf

-- | Collect the results of a nondeterministic computation in a tree.
allValues :: Tree a -> Tree a
allValues = id

infixr 0 -->

type a --> b = Tree a -> Tree b

instance
  (Normalform Tree a1 a2, Normalform Tree b1 b2) =>
  Normalform Tree (a1 --> b1) (a2 -> b2)
  where
  nf mf =
    mf >> return (error "Plugin Error: Cannot capture function types")
  liftE mf = do
    f <- mf
    return (liftE . fmap f . nf)

-- | Lift a unary function with the lifting scheme of the plugin.
liftTree1 :: (a -> b) -> Tree (a --> b)
liftTree1 f = rtrnFunc (\a -> a >>= \a' -> return (f a'))

-- | Lift a 2-ary function with the lifting scheme of the plugin.
liftTree2 :: (a -> b -> c) -> Tree (a --> b --> c)
liftTree2 f =
  rtrnFunc
    ( \a ->
        rtrnFunc
          ( \b ->
              a >>= \a' -> b >>= \b' -> return (f a' b')
          )
    )

-- | Apply a lifted 2-ary function to its lifted arguments.
apply2 :: Tree (a --> b --> c) -> Tree a -> Tree b -> Tree c
apply2 f a b = app f a >>= \f' -> f' b

-- | Apply a lifted 2-ary function to its arguments, where just the
-- first argument has to be lifted.
apply2Unlifted ::
  Tree (a --> b --> c) ->
  Tree a ->
  b ->
  Tree c
apply2Unlifted f a b = app f a >>= \f' -> f' (return b)

-- | Apply a lifted 3-ary function to its lifted arguments.
apply3 ::
  Tree (a --> b --> c --> d) ->
  Tree a ->
  Tree b ->
  Tree c ->
  Tree d
apply3 f a b c = apply2 f a b >>= \f' -> f' c
