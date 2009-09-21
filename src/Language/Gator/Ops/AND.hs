{-# LANGUAGE FlexibleContexts #-}
module Language.Gator.Ops.AND (
    doAnd,
    doAndN,
    (<&&>),
) where

import Control.Monad.State
import Language.Gator.Logic
import Language.Gator.IO
import Language.Gator.Gates
import Language.Gator.Gates.AND
import Language.Gator.Ops.General

nextAND :: (MonadState Logic m) => m Name
nextAND = do
    idx <- nextIdxOf andID
    return $ "and" ++ (show idx)

(<&&>) :: (Out a, Out b, MonadState Logic m) => a -> b -> m AND
a <&&> b = doAnd a b

doAnd :: (Out a, Out b, MonadState Logic m) => a -> b -> m AND
doAnd a b = do
    n <- nextAND
    doAndN n a b

newAndN :: (MonadState Logic m) => Name -> m AND
newAndN n = do
    i <- nextGateID

    let g = AND n i
        g' = G_AND g

    gateSets $ (modify $ (g':))
    return g

doAndN :: (Out a, Out b, MonadState Logic m) => Name -> a -> b -> m AND
doAndN = doOp2N newAndN
