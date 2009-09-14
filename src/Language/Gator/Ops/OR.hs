{-# LANGUAGE FlexibleContexts #-}
module Language.Gator.Ops.OR (
    newOr,
    newOrN,
    doOr,
    doOrN,
) where

import Control.Monad.State
import Language.Gator.Logic
import Language.Gator.General
import Language.Gator.IO
import Language.Gator.Gates.OR

import Language.Gator.Ops.General

import qualified Data.Set as S

nextOR :: (MonadState Logic m) => m Name
nextOR = do
    idx <- nextIdxOf orID
    return $ "or" ++ (show idx)

newOr :: (MonadState Logic m) => m OR
newOr = nextOR >>= newOrN 

doOr :: (Out a, Out b, MonadState Logic m) => a -> b -> m OR
doOr a b = do
    n <- nextOR
    doOrN n a b

newOrN :: (MonadState Logic m) => Name -> m OR
newOrN n = do
    (gateSets . orGates) $ (modify $ S.insert g)
    return g
    where
        g = OR n

doOrN :: (Out a, Out b, MonadState Logic m) => Name -> a -> b -> m OR
doOrN = doOp2N newOrN
