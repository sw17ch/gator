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
import Language.Gator.Gates.XOR

import Language.Gator.Ops.General

import qualified Data.Set as S

nextXOR :: (MonadState Logic m) => m Name
nextXOR = do
    idx <- nextIdxOf xorID
    return $ "xor" ++ (show idx)

newXOr :: (MonadState Logic m) => m XOR
newXOr = nextXOR >>= newXOrN

doXOr :: (Out a, Out b, MonadState Logic m) => a -> b -> m XOR
doXOr a b = do
    n <- nextXOR
    doXOrN n a b

newXOrN :: (MonadState Logic m) => Name -> m XOR
newXOrN n = do
    (gateSets . xorGates) $ (modify $ S.insert g)
    return g
    where
        g = XOR n

doXOrN :: (Out a, Out b, MonadState Logic m) => Name -> a -> b -> m XOR
doXOrN = doOp2N newXOrN
