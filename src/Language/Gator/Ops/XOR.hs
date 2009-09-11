{-# LANGUAGE FlexibleContexts #-}
module Language.Gator.Ops.XOR (
    newXOr,
    newXOrN,
    doXOr,
    doXOrN,
) where

import Control.Monad.State
import Language.Gator.Logic
import Language.Gator.General
import Language.Gator.IO
import Language.Gator.Gates.XOrGate

import Language.Gator.Ops.General

import qualified Data.Set as S

nextXOR :: (MonadState Logic m) => m Name
nextXOR = do
    idx <- nextIdxOf xorID
    return $ "xor" ++ (show idx)

newXOr :: (MonadState Logic m) => m XOrGate
newXOr = nextXOR >>= newXOrN

doXOr :: (Out a, Out b, MonadState Logic m) => a -> b -> m XOrGate
doXOr a b = do
    n <- nextXOR
    doXOrN n a b

newXOrN :: (MonadState Logic m) => Name -> m XOrGate
newXOrN n = do
    (gateSets . xorGates) $ (modify $ S.insert g)
    return g
    where
        g = XOrGate n

doXOrN :: (Out a, Out b, MonadState Logic m) => Name -> a -> b -> m XOrGate
doXOrN = doOp2N newXOrN
