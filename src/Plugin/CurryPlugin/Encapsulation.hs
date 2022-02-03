{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Plugin.CurryPlugin.Encapsulation
Description : Encapsulation of Nondeterminism
Copyright   : (c) Kai-Oliver Prott (2020)
License     : BSD-3 Clause
Maintainer  : kai.prott@hotmail.de

This module contains functions to encapsulate the nondeterminism of
plugin-compiled functions.
-}
module Plugin.CurryPlugin.Encapsulation
  ( Tree, SearchMode(..)
  , evalGeneric, evalN, eval, eval1, eval2
  ) where

import Plugin.CurryPlugin.THEval
import Plugin.CurryPlugin.Monad

-- | Evaluate a nullary nondeterministic function
-- with the given search strategy.
eval :: Normalform Tree a b
     => SearchMode -> Tree a -> [b]
eval = $(evalN 0)

-- | Evaluate a unary nondeterministic function
-- with the given search strategy and arguments
eval1 :: (Normalform Tree a1 a2, Normalform Tree b1 b2)
      => SearchMode -> Tree (a1 --> b1) -> a2 -> [b2]
eval1 = $(evalN 1)

-- | Evaluate a 2-ary nondeterministic function
-- with the given search strategy and arguments
eval2 :: ( Normalform Tree a1 a2
         , Normalform Tree b1 b2
         , Normalform Tree c1 c2)
      => SearchMode -> Tree (a1 --> b1 --> c1) -> a2 -> b2 -> [c2]
eval2 = $(evalN 2)
