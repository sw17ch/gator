{-# LANGUAGE FlexibleContexts #-}
module Language.Gator.Ops.OR (
    doOr,
    doOrN,
    (<||>),
) where

import Control.Monad.State
import Language.Gator.Logic
import Language.Gator.IO
import Language.Gator.Gates
import Language.Gator.Gates.OR

import Language.Gator.Ops.General

nextOR :: (MonadState Logic m) => m Name
nextOR = do
    idx <- nextIdxOf orID
    return $ "or" ++ (show idx)

(<||>) :: (Out a, Out b, MonadState Logic m) => a -> b -> m OR
a <||> b = doOr a b

doOr :: (Out a, Out b, MonadState Logic m) => a -> b -> m OR
doOr a b = do
    n <- nextOR
    doOrN n a b

newOrN :: (MonadState Logic m) => Name -> m OR
newOrN n = do
    i <- nextGateID

    let g  = OR n i
        g' = G_OR g

    gateSets $ modify (g':)
    return g

doOrN :: (Out a, Out b, MonadState Logic m) => Name -> a -> b -> m OR
doOrN = doOp2N newOrN
