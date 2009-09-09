{-# LANGUAGE FlexibleContexts #-}
module Language.Gator.Ops.XOR where

import Control.Monad.State
import Language.Gator.Logic
import Language.Gator.General
import Language.Gator.IO
import Language.Gator.Gates.XOrGate

import Language.Gator.Ops.NextIDX

import qualified Data.Map as M
import qualified Data.Set as S

nextXOR :: (MonadState Logic m) => m Name
nextXOR = do
    idx <- nextIdxOf xorID
    return $ "xor" ++ (show idx)

newXOr :: (MonadState Logic m) => Name -> m XOrGate
newXOr n = do
    (gateSets . xorGates) $ (modify $ S.insert g)
    return g
    where
        g = XOrGate n

doXOr :: (Out a, Out b, MonadState Logic m) => a -> b -> Name -> m XOrGate
doXOr a b n = do
    g <- newXOr n
    (joints) $ (modify $ js g)
    return g
    where
        js g = (M.insert (out b) (in1 g)) . (M.insert (out a) (in0 g))

