{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Language.Gator.Ops.NextIDX where

import Control.Monad.State
import Language.Gator.Logic
import Data.Lenses

nextIdxOf :: (MonadState Logic m, Num s, MonadState s n) =>
             (n s -> StateT GateIDs m b) -> m b
nextIdxOf f = (gateIDs . f) $ getAndModify (+1)

