{-# LANGUAGE FlexibleContexts #-}
module Language.Gator.Ops.Output (
    newOutput,
    newOutputN,
) where

import Language.Gator.Logic
import Language.Gator.General
import Language.Gator.Gates.Output
import Language.Gator.Ops.NextIDX

import Control.Monad.State
import qualified Data.Set as S

nextOutput :: (MonadState Logic m) => m Name
nextOutput = do
    idx <- nextIdxOf outputID
    return $ "out" ++ (show idx)

newOutput :: (MonadState Logic m) => m Output
newOutput = nextOutput >>= newOutputN

newOutputN :: (MonadState Logic m) => Name -> m Output
newOutputN n = do
    (gateSets . outputs) $ (modify $ S.insert g)
    return g
    where
        g = Output n

