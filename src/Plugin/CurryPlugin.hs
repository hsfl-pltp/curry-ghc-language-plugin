{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Plugin.CurryPlugin
Description : A GHC plugin to transform GHC into a Curry-Compiler.
Copyright   : (c) Kai-Oliver Prott (2020)
Maintainer  : kai.prott@hotmail.de

This module contains a GHC plugin that turns GHC into a "compiler" for
the functional-logic programming language Curry.
-}
module Plugin.CurryPlugin (plugin) where

import Data.List
import Data.IORef
import Data.Maybe
import Control.Exception
import Language.Haskell.TH (Extension(..))

import GHC.Hs.Binds
import GHC.Hs.Extension
import Plugins
import TcRnTypes
import TcEvidence
import GhcPlugins
import TcDeriv
import TcBinds
import TcInstDcls
import Bag
import TcSimplify
import TcEnv
import TcMType
import TcHsSyn
import TcRnMonad
import UniqMap
import InstEnv
import ErrUtils

import Plugin.Dump
import Plugin.Trans.Expr
import Plugin.Trans.Type
import Plugin.Trans.Util
import Plugin.Trans.Derive
import Plugin.Trans.ClsInst
import Plugin.Trans.TyCon
import Plugin.Trans.Import
import Plugin.Trans.TysWiredIn
import Plugin.Trans.ConstraintSolver
import Plugin.Trans.Preprocess
import Plugin.Trans.Class
import Plugin.Trans.Constr
import Plugin.Effect.Annotation

-- | This GHC plugin turns GHC into a "compiler" for
-- the functional-logic programming language Curry.
plugin :: Plugin
plugin = defaultPlugin
  { renamedResultAction   = const processImportPlugin
  , typeCheckResultAction = const . liftMonadPlugin . parseDumpOpts
  , pluginRecompile       = const (return NoForceRecompile)
  , tcPlugin              = const (Just conPlugin)
  , dynflagsPlugin        = const (return . (`xopt_unset` ImplicitPrelude))
  }
  where
    conPlugin = TcPlugin
      { tcPluginInit  = unsafeTcPluginTcM loadDefaultTyConMap
      , tcPluginSolve = tcPluginSolver
      , tcPluginStop  = const (return ())
      }

-- | This type checker plugin implements the lifting of declarations
-- for the Curry plugin.
liftMonadPlugin :: Maybe DumpOpts -> TcGblEnv -> TcM TcGblEnv
liftMonadPlugin mdopts env = do
  dopts <- case mdopts of
    Just xs -> return xs
    Nothing -> addErrTc "Error! Unrecognized plugin option" >>
               failIfErrsM >> return mempty

  dumpWith DumpOriginal        dopts (tcg_binds    env)
  dumpWith DumpOriginalEv      dopts (tcg_ev_binds env)
  dumpWith DumpOriginalInstEnv dopts (tcg_inst_env env)
  dumpWith DumpOriginalTypeEnv dopts (tcg_type_env env)

  -- remove any dummy evidence introduced by the constraint solver plugin
  let tcg_ev_binds' = filterBag (not . isDummyEv) (tcg_ev_binds env)

  hsc <- getTopEnv
  mapRef <- loadDefaultTyConMap
  let tyconsMap = (hsc, mapRef)

  -- lift datatypes, we need the result for the lifting of datatypes itself
  s <- getUniqueSupplyM
  mtycon <- getMonadTycon
  stycon <- getShareClassTycon
  res <- liftIO ((mdo
    liftedTycns <- snd <$>
      mapAccumM (\s' t -> liftTycon stycon mtycon s' tnsM tyconsMap t)
        s (tcg_tcs env)
    let tycns = mapMaybe (\(a,b) -> fmap (a,) b) liftedTycns
    let tnsM = listToUniqMap tycns
    return (Right (tycns, liftedTycns)))
    `catch` (\e -> return (Left e)))

  -- extrect results or analyze any thrown IO errors
  (tycons, liftedTycons) <- case res of
    Left e | Just (ClassLiftingException cls reason) <- fromException e
            -> do
              flags <- getDynFlags
              let l = srcLocSpan (nameSrcLoc (getName cls))
              reportError (mkErrMsg flags l neverQualify (text reason))
              failIfErrsM
              return ([], [])
           | Just (RecordLiftingException _ p reason) <- fromException e
            -> do
              flags <- getDynFlags
              let l = srcLocSpan (nameSrcLoc (getName p))
              reportError (mkErrMsg flags l neverQualify (text reason))
              failIfErrsM
              return ([], [])
           | otherwise
            -> failWith (text ("Unknown error occurred during lifting:" ++
                                displayException e))
    Right r -> return r

  let new = map snd tycons
  -- The order is important,
  -- as we want to keep t2 if it has the same unique as t1.
  let getRelevant (t1, Just t2) = if t1 == t2 then [t2] else [t1, t2]
      getRelevant (t1, Nothing) = [t1]
  let tcg_tcs' = concatMap getRelevant liftedTycons
  -- insert new tycons mapping into mapRef
  liftIO $ modifyIORef mapRef (insertNewTycons liftedTycons)

  -- insert the new ones into the rename environment
  let rdr = createRdrEnv new `plusGlobalRdrEnv` tcg_rdr_env env

  -- generate module annotation
  let a = Annotation (ModuleTarget (tcg_semantic_mod env))
            (toSerialized serializeWithData Nondeterministic)

  -- update environment and remove tc plugins temporarily
  let aenv = tcg_ann_env env
  let anns = tcg_anns env
  let aenv' = extendAnnEnvList aenv [a]
  let anns' = a : anns
  let tenv = plusTypeEnv (tcg_type_env env) (typeEnvFromEntities [] tcg_tcs' [])
  writeTcRef (tcg_type_env_var env) tenv
  setGblEnv (env { tcg_tcs        = tcg_tcs'
                 , tcg_type_env   = tenv
                 , tcg_ann_env    = aenv'
                 , tcg_anns       = anns'
                 , tcg_rdr_env    = rdr
                 , tcg_ev_binds   = tcg_ev_binds'
                 , tcg_tc_plugins = [] }) $ do

    -- set temporary flags needed for all further steps
    -- (enable some language extentions and disable all warnings)
    flags <- getDynFlags
    setDynFlags (flip (foldl wopt_unset) [toEnum 0 ..] $
                 flip (foldl xopt_set) requiredExtensions $
                 (flags { cachedPlugins = [], staticPlugins = [] })) $ do

      -- gather neccessary derivings
      derivs <- mkDerivings tycons
      -- check and rename those derivings
      (env1, infos, derivBinds) <- tcDeriving [] derivs
      setGblEnv env1 $ do
        -- create all instances from those derivings
        ((env2, lcl, derivedBindings), wc) <- captureTopConstraints $ do
          bs <- tcInstDecls2 [] (bagToList infos)
          -- typecheck other bindings that resulted from those derivings
          (e,l) <- uncurry tcTopBinds (collectBind derivBinds)
          return (e, l, bs)

        -- Solve the constraints
        wc' <- zonkWC wc
        ev <- unionBags ( tcg_ev_binds env2 ) <$> simplifyTop wc'

        -- zonk all evidence and new decls
        (tenv2, ev', bs', _, _, _) <- zonkTopDecls ev derivedBindings [] [] []

        -- Check if deriving generated an error.
        errsVar <- getErrsVar
        msgs <- readTcRef errsVar
        dflags <- getDynFlags
        if errorsFound dflags msgs
          -- Add error message if deriving failed and
          -- suppress advanced infos, unless a debug option is set.
          then if DumpDerivingErrs `elem` d_phases dopts
            then do
              addErrTc "The Curry-Plugin failed to derive internal instances."
              failIfErrsM
              return env
            else do
              writeTcRef errsVar (emptyBag, emptyBag)
              failWithTc $ "The Curry-Plugin failed to lift the" <+>
                           "definitions in this module." $+$
                           "Did you use any unsupported language extension?" $+$
                           "To see all internal errors, use the flag" $$
                           "'-fplugin-opt" <+>
                           "Plugin.CurryPlugin:dump-deriving-errs'"
          -- If everything is ok, just continue as planned.
          else do

            -- update the env, but do not add derived bs',
            -- as they should not be lifted
            let tenv3 = plusTypeEnv tenv tenv2
            writeTcRef (tcg_type_env_var env2) tenv3
            let env3 = env2 { tcg_ev_binds = ev', tcg_type_env = tenv3 }
            setGblEnv env3 $ setLclEnv lcl $ do

              -- compile pattern matching
              prep <- bagToList <$>
                liftBag (preprocessBinding False) (tcg_binds env3)
              dumpWith DumpPatternMatched dopts prep

              -- lift instance information
              let origInsts = tcg_insts env
              newInsts <- mapM (liftInstance tyconsMap) origInsts

              -- Remove all instances that were defined in this module
              -- from all instances that were created during compilation,
              -- and replace them with the new instances.
              let allInsts = deleteFirstsBy ((. is_cls_nm) . (==) . is_cls_nm)
                    (tcg_insts env3) origInsts ++ newInsts
              -- For the environment, we have to keep all external instances,
              -- while replacing all local instances with the new ones.
              -- So we do the same as above,
              -- but use tcg_inst_env instead of tcg_insts.
              let newInstEnv = extendInstEnvList emptyInstEnv
                    (deleteFirstsBy ((. is_cls_nm) . (==) . is_cls_nm)
                      (instEnvElts (tcg_inst_env env3)) origInsts ++ newInsts)
              dumpWith DumpInstEnv dopts newInstEnv
              let env4 = env3 { tcg_insts = allInsts
                              , tcg_inst_env = newInstEnv}
              setGblEnv env4 $ do

                -- finally do the monadic lifting for functions and dicts
                tcg_binds' <- liftBindings tyconsMap newInsts prep

                -- create the final environment with restored plugin field
                let finalEnv = env4 { tcg_binds      = listToBag tcg_binds'
                                    , tcg_tc_plugins = tcg_tc_plugins env
                                    , tcg_ev_binds   = emptyBag
                                    }
                      `addTypecheckedBinds` [bs']

                return finalEnv
  where
    liftBindings :: TyConMap -> [ClsInst] -> [LHsBindLR GhcTc GhcTc]
                 -> TcM [LHsBindLR GhcTc GhcTc]
    liftBindings y z = fmap (map noLoc) .
      concatMapM (fmap fst . liftMonadicBinding False False [] y z . unLoc)

    collectBind (ValBinds _ b s)              = ([(Recursive, b)], s)
    collectBind (XValBindsLR (NValBinds b s)) = (b, s)

    isDummyEv (EvBind _ (EvExpr (Var v)) _) =
                  occNameString (occName v) == "#dummy_remove"
    isDummyEv _ = False

-- | Create a RdrEnv from the given list of type constructors.
-- It can be used to look up names and their origin.
createRdrEnv :: [TyCon] -> GlobalRdrEnv
createRdrEnv = mkGlobalRdrEnv . concatMap createEntries
  where
    createEntries tc = p : concatMap conEntries (tyConDataCons tc)
      where
        n = tyConName tc
        p = GRE n NoParent True []
        conEntries c = GRE (dataConName c) (ParentIs n) True [] :
                       map fieldEntry (dataConFieldLabels c)
        fieldEntry f = GRE (flSelector f) (FldParent n (Just (flLabel f)))
                         True []

-- | Insert the given list of type constructors into the TyConMap.
insertNewTycons :: [(TyCon, Maybe TyCon)]
                -> ( UniqMap TyCon TyCon
                   , UniqMap TyCon TyCon
                   , UniqSet TyCon
                   , UniqSet TyCon )
                -> ( UniqMap TyCon TyCon
                   , UniqMap TyCon TyCon
                   , UniqSet TyCon
                   , UniqSet TyCon )
insertNewTycons = flip (foldr insertNew)
  where
    insertNew (tc, mbtc) (m1, m2, s1, s2) =
      (maybe m1 (addToUniqMap m1 tc) mbtc,
       maybe m2 (flip (addToUniqMap m2) tc) mbtc,
       addOneToUniqSet s1 tc,
       maybe s2 (addOneToUniqSet s2) mbtc)

-- | Extensions that are required by the plugin.
requiredExtensions :: [Extension]
requiredExtensions =
  [ DeriveGeneric
  , DeriveAnyClass
  , EmptyDataDeriving
  , StandaloneDeriving
  , FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  , MultiParamTypeClasses
  , TypeFamilies
  , QuantifiedConstraints
  , RankNTypes ]
