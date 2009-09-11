{-# LANGUAGE FlexibleContexts #-}
module Language.Gator.Ops.AND (
    newAnd,
    newAndN,
    doAnd,
    doAndN,
) where

import Control.Monad.State
import Language.Gator.Logic
import Language.Gator.General
import Language.Gator.IO
import Language.Gator.Gates.AndGate

import Language.Gator.Ops.General

import qualified Data.Set as S

nextAND :: (MonadState Logic m) => m Name
nextAND = do
    idx <- nextIdxOf andID
    return $ "and" ++ (show idx)

newAnd :: (MonadState Logic m) => m AndGate
newAnd = nextAND >>= newAndN

doAnd :: (Out a, Out b, MonadState Logic m) => a -> b -> m AndGate
doAnd a b = do
    n <- nextAND
    doAndN n a b

newAndN :: (MonadState Logic m) => Name -> m AndGate
newAndN n = do
    (gateSets . andGates) $ (modify $ S.insert g)
    return g
    where
        g = AndGate n

doAndN :: (Out a, Out b, MonadState Logic m) => Name -> a -> b -> m AndGate
doAndN = doOp2N newAndN
