{-# LANGUAGE FlexibleContexts #-}
module Language.Gator.Ops.Output (
    newOutput,
    newOutputN,
) where

import Language.Gator.Logic
import Language.Gator.General
import Language.Gator.Gates
import Language.Gator.Gates.Output
import Language.Gator.Ops.General

import Control.Monad.State

nextOutput :: (MonadState Logic m) => m Name
nextOutput = do
    idx <- nextIdxOf outputID
    return $ "out" ++ (show idx)

newOutput :: (MonadState Logic m) => m Output
newOutput = nextOutput >>= newOutputN

newOutputN :: (MonadState Logic m) => Name -> m Output
newOutputN n = do
    gateSets $ modify (g':)
    return g
    where
        g = Output n
        g' = G_Output g

