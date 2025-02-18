{-# LANGUAGE TupleSections #-}
{-|
Module      : Plugin.Trans.DictInstFun
Description : Lift the dictionary binding of a type class.
Copyright   : (c) Kai-Oliver Prott (2020)
Maintainer  : kai.prott@hotmail.de

This module contains a function to lift the dictionary binding of a type
class. It is not lifted like a normal function.
-}
module Plugin.Trans.DictInstFun (liftDictInstFun) where

import Data.List
import Data.Maybe
import Control.Monad
import Language.Haskell.Syntax.Extension
import GHC.Parser.Annotation

import GHC.Plugins
import GHC.Hs.Binds
import GHC.Hs.Extension
import GHC.Hs.Expr
import GHC.Tc.Types
import GHC.Tc.Types.Evidence
import GHC.Core.TyCo.Rep
import GHC.Core.ConLike
import GHC.Core.InstEnv
import GHC.Data.Bag

import Plugin.Trans.Type
import Plugin.Trans.Util
import Plugin.Trans.Constr
import Plugin.Trans.Var

-- | Lift the dictionary binding of a type class.
-- It is not lifted like a normal function.
liftDictInstFun :: HsBindLR GhcTc GhcTc -> TyConMap
                -> [ClsInst] -> TcM (Maybe (HsBindLR GhcTc GhcTc))
liftDictInstFun bind tcs clsInsts = do
  -- Find the correct binding we have to lift from the potential AbsBinds.
  let exported = if isAbsBinds bind then map abe_poly (abs_exports bind) else []
  -- Find the corresponding lifted instance information.
  case find ((`elem` exported) . is_dfun) clsInsts of
    Nothing  -> panicAny "Could not find lifted instance info" exported
    Just new -> liftDictInstBinding tcs new bind -- Perform the actual lifting.

-- | Lift the dictionary binding of a type class
-- by using its lifted instance info.
-- It is not lifted like a normal function.
liftDictInstBinding :: TyConMap -> ClsInst -> HsBindLR GhcTc GhcTc
                    -> TcM (Maybe (HsBindLR GhcTc GhcTc))
liftDictInstBinding tcs cls (AbsBinds _ tvs evs ex evb bs sig)
  | [L _ (VarBind _ _ e)] <- bagToList bs,
    [ABE _ _ m w s] <- ex = do
      -- The new poly var is the one from the given clsInst.
      let p' = is_dfun cls
      -- The monomorphic one has to be updated for the new type constructors.
      m' <- setVarType m <$> liftIO (replaceTyconTy tcs (varType m))

      -- Create the dictionary variables.
      stc <- getShareClassTycon
      mty <- mkTyConTy <$> getMonadTycon
      u   <- replicateM (length tvs) getUniqueSupplyM
      let mkShareTy ty = mkTyConApp stc [mty, ty]
      let evsty = catMaybes $
                  zipWith ((. flip Bndr Inferred) . mkShareable mkShareTy) u tvs
      newevs <- mapM freshDictId evsty

      -- Each function and superclass selector uses the same wrapper,
      -- that applies all tvs and then all evs.
      -- So we create it once, and use it everywhere.
      let replaceEv ev = setVarType ev <$> replaceTyconTy tcs (varType ev)
      allevsids <- (++ newevs) <$> liftIO (mapM replaceEv evs)
      let evWraps = map (WpEvApp . EvExpr . evId) allevsids
      let tyWraps = map (WpTyApp . mkTyVarTy) tvs
      let wrap = foldl (<.>) WpHole (reverse evWraps ++ reverse tyWraps)

      -- Lift the actual expression.
      e' <- liftDictExpr cls wrap tcs e

      let vb = listToBag [noLocA (VarBind noExtField m' e')]
      let ex' = [ABE noExtField p' m' w s]
      let b' = AbsBinds noExtField tvs allevsids ex' evb vb sig
      return (Just b')
liftDictInstBinding _ _ _ = return Nothing

-- | Lidt the expression of a dictionary binding.
liftDictExpr :: ClsInst -> HsWrapper -> TyConMap -> LHsExpr GhcTc
             -> TcM (LHsExpr GhcTc)
liftDictExpr cls w tcs (L l ex) = L l <$> liftDictExpr' ex
  where
    -- Applications are translated by lifting both sides.
    liftDictExpr' (HsApp _ e1 e2) =
      HsApp EpAnnNotUsed <$> liftDictExpr cls w tcs e1
                         <*> liftDictExpr cls w tcs e2
    -- The dictionary constructor is lifted by getting the lifted constructor
    -- and lifting its wrapper.
    liftDictExpr' (XExpr (WrapExpr
      (HsWrap cw (HsConLikeOut _ (RealDataCon dc))))) = do
      cw' <- liftIO (replaceWrapper tcs cw)
      dc' <- liftIO (getLiftedCon dc tcs)
      return (XExpr (WrapExpr
        (HsWrap cw' (HsConLikeOut noExtField (RealDataCon dc')))))
    liftDictExpr' (HsConLikeOut _ (RealDataCon dc)) = do
      dc' <- liftIO (getLiftedCon dc tcs)
      return (HsConLikeOut noExtField (RealDataCon dc'))
    liftDictExpr' (HsVar _ (L _ v)) = liftInstFunUse v
    -- Other wrappers are discarded and re-create at liftInstFunUse.
    liftDictExpr' (XExpr (WrapExpr (HsWrap _ e))) = liftDictExpr' e
    liftDictExpr' e = panicAny "Unexpected expression in dictionary function" e

    liftInstFunUse :: Var -> TcM (HsExpr GhcTc)
    liftInstFunUse v = do
      stc <- getShareClassTycon
      mtc <- getMonadTycon

      -- v has type forall instVars . instConstraints => clsFuncTy,
      -- where clsFuncTy might start with a forall itself.
      -- Instead of putting all Shareable constraints after all foralls,
      -- we want to place them behind instConstraints.
      -- So we split off as many foralls as there are instVars and
      -- then split off as many invisible function args as possible.
      -- But we only do all of this, if the type is not already lifted
      let ty = varType v
      dfLifted <- case splitTyConApp_maybe (snd (splitInvisPiTys ty)) of
        Just (tc, _) | tc == mtc
          -> setVarType v <$> liftIO (replaceTyconTy tcs ty)
        _ -> do
          let (bs1, ty1) = splitInvisPiTysN (length (is_tvs cls)) ty
              (bs2, ty2) = splitInvisFunTys ty1
              bs = mapMaybe namedTyCoVarBinder_maybe bs1
          uss <- replicateM (length bs) getUniqueSupplyM
          let mkShareType t' = mkTyConApp stc [mkTyConTy mtc, t']
              cons = catMaybes $ zipWith (mkShareable mkShareType) uss bs
          bs' <- liftIO (mapM (replacePiTy tcs) (bs1 ++ bs2))
          ty' <- mkPiTys bs' . flip (foldr mkInvisFunTyMany) cons
            <$> liftTypeTcM tcs ty2
          return (setVarType v ty')

      -- Use the given wrapper expression.
      return (XExpr (WrapExpr
        (HsWrap w (HsVar noExtField (noLocA dfLifted)))))

-- | Split off all arguments of an invisible function type
-- (e.g., all constraints).
splitInvisFunTys :: Type -> ([TyCoBinder], Type)
splitInvisFunTys (FunTy InvisArg m ty1 ty2) =
  let (bs, ty2') = splitInvisFunTys ty2
  in (Anon InvisArg (Scaled m ty1) : bs, ty2')
splitInvisFunTys ty = ([], ty)

-- | Test iff a binding is an AbsBind.
isAbsBinds :: HsBindLR idL idR -> Bool
isAbsBinds AbsBinds{} = True
isAbsBinds _          = False
