{-# OPTIONS_GHC "-Wno-orphans" #-}
{-|
Module      : Plugin.Trans.Util
Description : Various utility functions
Copyright   : (c) Kai-Oliver Prott (2020)
Maintainer  : kai.prott@hotmail.de

This module contains various utility functions.
-}
module Plugin.Trans.Util where

import Language.Haskell.TH            ( Exp, Q, runQ )
import Language.Haskell.Syntax.Extension
import Control.Monad.IO.Class
import Data.Tuple.Extra
import Data.List
import Data.List.Split
import Data.Typeable
import Data.ByteString                ( unpack )

import GHC.HsToCore
import GHC.ThToHs
import GHC.Plugins
import GHC.Hs.Extension
import GHC.Hs.Expr
import GHC.Hs.Lit
import GHC.Tc.Types
import GHC.Tc.Gen.Expr
import GHC.Tc.Types.Evidence
import GHC.Tc.Utils.Monad
import GHC.Core.TyCo.Rep
import GHC.Unit.Finder
import GHC.Rename.Expr
import GHC.Data.Bag
import GHC.Types.SourceText
import GHC.Parser.Annotation

namedTyCoVarBinder_maybe :: TyCoBinder -> Maybe TyCoVarBinder
namedTyCoVarBinder_maybe (Named v) = Just v
namedTyCoVarBinder_maybe _         = Nothing

-- | Lift a computation from the 'Q' monad to the type checker monad.
liftQ :: Q a -> TcM a
liftQ = liftIO . runQ

findImportedOrPanic :: String -> TcM Module
findImportedOrPanic mname = do
  hscEnv <- getTopEnv
  res <- liftIO $ findImportedModule hscEnv (mkModuleName mname) Nothing
  case res of
    Found _ mdl -> return mdl
    _           -> panicAny "Could not find module" mname

findImportedPkgOrPanic :: String -> String -> TcM Module
findImportedPkgOrPanic mname pkgname = do
  hscEnv <- getTopEnv
  let strippedPkg = intercalate "-" $ init $ init $ wordsBy (=='-') pkgname
  res <- liftIO $ findImportedModule hscEnv (mkModuleName mname) Nothing
  case res of
    Found _ mdl -> return mdl
    _ -> do
      res2 <- liftIO $ findImportedModule hscEnv (mkModuleName mname)
                        (Just (mkFastString strippedPkg))
      case res2 of
        Found _ mdl -> return mdl
        _           -> panicAny "Could not find module"
                        (mkFastString (strippedPkg ++ ":" ++ mname))

-- | Convert a given TemplateHaskell expression into GHC's representation
-- and type check it against the given type.
mkNewAny :: Exp -> Type -> TcM (LHsExpr GhcTc)
mkNewAny ex ty = do
  ps_expr <- case convertToHsExpr Generated noSrcSpan ex of
    Left  msg -> do
      flags <- getDynFlags
      panic ("Error while converting TemplateHaskell: " ++ showSDoc flags msg)
    Right res -> return res
  fmap fst (rnLExpr ps_expr) >>= flip tcCheckMonoExpr ty

mkNewPs :: RdrName -> Type -> TcM (LHsExpr GhcTc)
mkNewPs nm ty = do
  let ps_expr = noLocA (HsVar noExtField (noLocA nm))
  fmap fst (rnLExpr ps_expr) >>= flip tcCheckMonoExpr ty

-- | Get the type of the given expression or return Nothing
-- if its type annotations are inconsistent.
getType :: LHsExpr GhcTc -> TcM (Maybe Type)
getType e = do
  hs_env <- getTopEnv
  (_, mbe) <- liftIO (deSugarExpr hs_env e)
  return (exprType <$> mbe)

-- | Get the type of the given expression or panic
-- if its type annotations are inconsistent.
getTypeOrPanic :: LHsExpr GhcTc -> TcM Type
getTypeOrPanic e = do
  mty <- getType e
  case mty of
    Just ty -> return ty
    Nothing -> panicAny "Could not get type of expression" e

-- | Print an outputable "thing" under the given string tag.
printAny :: (MonadIO m, HasDynFlags m, Outputable a) => String -> a -> m ()
printAny str a = do
  liftIO $ putStr str
  liftIO $ putStr ": "
  getDynFlags >>= liftIO . putStrLn . flip showPpr a

-- | Creates a compiler panic with the given string tag and outputable.
-- Also aborts compilation.
panicAny :: (MonadIO m, HasDynFlags m, Outputable a, Typeable a)
         => String -> a -> m b
panicAny str a = do
  s <- flip showPpr a <$> getDynFlags
  if typeOf a == typeOf ()
    then panic (str ++ ".")
    else panic (str ++ ": " ++ s)

-- | Creates a compiler panic with the given string tag and outputable
-- in a context without safe acess to DynFlags.
-- Also aborts compilation.
panicAnyUnsafe :: (Outputable a, Typeable a) => String -> a -> b
panicAnyUnsafe str a =
  if typeOf a == typeOf ()
    then panic (str ++ ".")
    else panic (str ++ ": " ++ showPprUnsafe a)

-- | Creates a compiler panic with the given string tag and binder.
-- Also aborts compilation.
panicBndr :: (MonadIO m, HasDynFlags m, OutputableBndr a) => String -> a -> m b
panicBndr str a = do
  s <- flip showSDoc (pprBndr LetBind a) <$> getDynFlags
  panic (str ++ ": " ++ s)

-- | Creates a compiler panic with the given string tag and binder.
-- Also aborts compilation.
panicBndrUnsafe :: (OutputableBndr a) => String -> a -> b
panicBndrUnsafe str a = panic $
  str ++ ": " ++ renderWithContext defaultSDocContext (pprBndr LetBind a)

-- | Print an outputable "thing" under the given string tag in a context that
-- has no "safe" access to the global compiler flags.
printAnyUnsafe :: (MonadIO m, Outputable a) => String -> a -> m ()
printAnyUnsafe str a = do
  liftIO $ putStr str
  liftIO $ putStr ": "
  liftIO $ putStrLn $ showPprUnsafe a

-- | Print an outputable binder under the given string tag in a context that
-- has no "safe" access to the global compiler flags.
printBndrUnsafe :: (MonadIO m, OutputableBndr a) => String -> a -> m ()
printBndrUnsafe str a = do
  liftIO $ putStr str
  liftIO $ putStr ": "
  liftIO $ putStrLn $ renderWithContext defaultSDocContext (pprBndr LetBind a)

-- |Apply a monadic action to all elements in a bag with source location
-- annotations.
liftBag :: Monad m => (a -> m b)
        -> Bag (GenLocated l a) -> m (Bag (GenLocated l b))
liftBag = mapBagM . liftL

-- | Temporarily set the given global compiler flags for the excecution of the
-- given action. The flags are reset as soon as the action is finished.
setDynFlags :: DynFlags -> TcM a -> TcM a
setDynFlags f = updEnv (\(Env a b c d) -> Env (a { hsc_dflags = f }) b c d)

-- | Collect all type applications that are performed
-- by the given wrapper expression.
collectTyApps :: HsWrapper -> ([Type], [Var])
collectTyApps = flip collectTyApps' ([], [])
  where
    -- (w1 `compose` w2) e --> w1 (w2 e)
    -- => every type application in w2 is applied before w1,
    -- hence the "reversed" order here
    collectTyApps' (WpCompose w1 w2) = collectTyApps' w2 . collectTyApps' w1
    collectTyApps' (WpTyApp      ty) = first (ty:)
    collectTyApps' (WpTyLam       v) = second (v:)
    collectTyApps'  _                = id

-- | Get a list of all arguments of the given arithmetic sequence.
arithSeqArgs :: ArithSeqInfo GhcTc -> [LHsExpr GhcTc]
arithSeqArgs (From       f    ) = [f      ]
arithSeqArgs (FromThen   f n  ) = [f, n   ]
arithSeqArgs (FromTo     f   t) = [f,    t]
arithSeqArgs (FromThenTo f n t) = [f, n, t]

-- | Check if the given literal is a (primitive) string literal.
isStringLit :: HsLit p -> Bool
isStringLit (HsString     _ _) = True
isStringLit (HsStringPrim _ _) = True
isStringLit _                  = False

-- | Get the source text (not value) of the given (primitive) string literal.
stringLitSourceText :: Typeable p => HsLit (GhcPass p) -> SourceText
stringLitSourceText (HsString     s _) = s
stringLitSourceText (HsStringPrim s _) = s
stringLitSourceText l                  =
  panicAnyUnsafe "Not a string literal" l

-- | Get the value of the given (primitive) string literal as a 'FastString'.
stringLitFastString :: Typeable p => HsLit (GhcPass p) -> FastString
stringLitFastString (HsString     _ s) = s
stringLitFastString (HsStringPrim _ s) = mkFastStringByteList (unpack s)
stringLitFastString l                  =
  panicAnyUnsafe "Not a string literal" l

-- | Like StateT but with return tuple swapped
newtype StateM s m a = StateM { runStateM :: s -> m (s, a) }

instance Functor m => Functor (StateM s m) where
    fmap f (StateM x) = StateM $ \s -> fmap (second f) (x s)

instance Monad m => Applicative (StateM s m) where
    pure x = StateM $ \s -> return (s, x)
    StateM f <*> StateM x = StateM $ \s -> do (s', f') <- f s
                                              (s'', x') <- x s'
                                              return (s'', f' x')

-- | Monadic variant of 'mapAccumL'.
mapAccumM :: (Monad m, Traversable t)
          => (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
mapAccumM f s t = runStateM (traverse (\x -> StateM (`f` x)) t) s

instance NamedThing RecSelParent where
  getName (RecSelData   tc) = getName tc
  getName (RecSelPatSyn ps) = getName ps
