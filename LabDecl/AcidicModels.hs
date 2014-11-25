{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module LabDecl.AcidicModels where

import Data.Acid (makeAcidic)
import Language.Haskell.TH

import qualified LabDecl.Models as M
import LabDecl.Types (Database)

-- | This splice wraps the functions in M.eventNames with
-- M.makeAcidUpdate or M.makeAcidQuery so that they can be made
-- acidic.
$(
  let analyseFunc (argCount, AppT (AppT ArrowT a) b) = analyseFunc (succ argCount, b)
      analyseFunc (argCount, ConT con) | con == ''M.IUpdate = (argCount, True)
      analyseFunc (argCount, AppT (ConT con) a) | con == ''M.IQuery = (argCount, False)
      makeOwl n
        | n == 1 = VarE '(.)
        | n >= 2 = InfixE (Just (makeOwl 1)) (makeOwl 1) (Just (makeOwl (n-1)))
      makeWrapFunc n wf f
        | n == 0 = AppE (VarE wf) (VarE f)
        | n >= 1 = AppE (AppE (makeOwl n) (VarE wf)) (VarE f)
      autoMakeWrapper func = do
        VarI _ typ _ _ <- reify func
        let (argCount, isUpdate) = analyseFunc (0, typ)
        let wrapFunc = if isUpdate then 'M.makeAcidUpdate else 'M.makeAcidQuery
        let def = makeWrapFunc argCount wrapFunc func
        let newFunc = mkName (nameBase func)
        return [ValD (VarP newFunc) (NormalB def) []]
  in fmap concat . mapM autoMakeWrapper $ M.eventNames
 )

-- | This splice makes the new functions acidic.
$(makeAcidic ''Database . map (mkName . nameBase) $ M.eventNames)
