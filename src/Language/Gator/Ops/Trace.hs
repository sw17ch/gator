{-# LANGUAGE FlexibleContexts #-}
module Language.Gator.Ops.Trace (
    newTrace,
    newTraceN,
    traceTo,
    traceToN,
) where

import Control.Monad.State
import Language.Gator.Logic
import Language.Gator.General
import Language.Gator.IO
import Language.Gator.Gates.Trace

import Language.Gator.Ops.NextIDX

import qualified Data.Map as M
import qualified Data.Set as S

nextTrace :: (MonadState Logic m) => m Name
nextTrace = do
    idx <- nextIdxOf traceID
    return $ "trace" ++ (show idx)

newTrace :: (MonadState Logic m) => m Trace
newTrace = nextTrace >>= newTraceN

traceTo :: (Out a, In0 b, MonadState Logic m) => a -> b -> m Trace
traceTo a b = do
    n <- nextTrace
    traceToN n a b

newTraceN :: (MonadState Logic m) => Name -> m Trace
newTraceN n = do
    (gateSets . traces) $ (modify $ S.insert g)
    return g 
    where
        g = Trace n

traceToN :: (Out a, In0 b, MonadState Logic m) => Name -> a -> b -> m Trace
traceToN n a b = do
    g <- newTraceN n
    (joints) $ (modify $ js g)
    return g 
    where
        js g = (M.insert (out g) (in0 b)) . (M.insert (out a) (in0 g))
