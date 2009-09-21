{-# LANGUAGE FlexibleContexts #-}
module Language.Gator.Ops.XOR (
    doXOr,
    doXOrN,
    (<^^>),
) where

import Control.Monad.State
import Language.Gator.Logic
import Language.Gator.IO
import Language.Gator.Gates
import Language.Gator.Gates.XOR

import Language.Gator.Ops.General

nextXOR :: (MonadState Logic m) => m Name
nextXOR = do
    idx <- nextIdxOf xorID
    return $ "xor" ++ (show idx)

(<^^>) :: (Out a, Out b, MonadState Logic m) => a -> b -> m XOR
a <^^> b = doXOr a b

doXOr :: (Out a, Out b, MonadState Logic m) => a -> b -> m XOR
doXOr a b = do
    n <- nextXOR
    doXOrN n a b

newXOrN :: (MonadState Logic m) => Name -> m XOR
newXOrN n = do
    i <- nextGateID

    let g  = XOR n i
        g' = G_XOR g

    gateSets $ modify (g':)
    return g

doXOrN :: (Out a, Out b, MonadState Logic m) => Name -> a -> b -> m XOR
doXOrN = doOp2N newXOrN
