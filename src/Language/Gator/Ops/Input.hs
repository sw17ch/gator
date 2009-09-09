{-# LANGUAGE FlexibleContexts #-}
module Language.Gator.Ops.Input (
    newInput,
    newInputN,
) where

import Language.Gator.Logic
import Language.Gator.General
import Language.Gator.Gates.Input
import Language.Gator.Ops.NextIDX

import Control.Monad.State
import qualified Data.Set as S


nextInput :: (MonadState Logic m) => m Name
nextInput = do
    idx <- nextIdxOf inputID
    return $ "in" ++ (show idx)

newInput :: (MonadState Logic m) => m Input
newInput = nextInput >>= newInputN

newInputN :: (MonadState Logic m) => Name -> m Input
newInputN n = do
    (gateSets . inputs) $ (modify $ S.insert g)
    return g
    where
        g = Input n

