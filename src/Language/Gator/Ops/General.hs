{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Language.Gator.Ops.General (
    nextIdxOf,
    doOp2N,
) where

import Control.Monad.State
import Data.Lenses

import qualified Data.Map as M

import Language.Gator.General
import Language.Gator.Logic
import Language.Gator.IO

nextIdxOf :: (MonadState Logic m, Num s, MonadState s n) =>
             (n s -> StateT GateIDs m b) -> m b
nextIdxOf f = (gateIDs . f) $ getAndModify (+1)

doOp2N :: (In0 c, In1 c, Out a, Out b, MonadState Logic m) =>
          (Name -> m c) -> Name -> a -> b -> m c
doOp2N f n a b = do
    g <- f n
    (joints) $ (modify $ js g)
    return g
    where
        iw = M.insertWith (flip (++))
        js g = (iw (out b) [(in1 g)]) . (iw (out a) [(in0 g)])
