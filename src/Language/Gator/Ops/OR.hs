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
import Language.Gator.Gates.OrGate

import Language.Gator.Ops.NextIDX

import qualified Data.Map as M
import qualified Data.Set as S

nextOR :: (MonadState Logic m) => m Name
nextOR = do
    idx <- nextIdxOf orID
    return $ "or" ++ (show idx)

newOr :: (MonadState Logic m) => m OrGate
newOr = do
    n <- nextOR
    newOrN n

doOr :: (Out a, Out b, MonadState Logic m) => a -> b -> m OrGate
doOr a b = do
    n <- nextOR
    doOrN n a b

newOrN :: (MonadState Logic m) => Name -> m OrGate
newOrN n = do
    (gateSets . orGates) $ (modify $ S.insert g)
    return g
    where
        g = OrGate n

doOrN :: (Out a, Out b, MonadState Logic m) => Name -> a -> b -> m OrGate
doOrN n a b = do
    g <- newOrN n
    (joints) $ (modify $ js g)
    return g
    where
        js g = (M.insert (out b) (in1 g)) . (M.insert (out a) (in0 g))
