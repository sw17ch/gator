{-# LANGUAGE FlexibleContexts #-}
module Language.Gator.Ops.Trace (
    newTrace,
    newTraceN,
    traceTo,
    traceToN,
) where

import Control.Monad.State
import Language.Gator.Logic
import Language.Gator.IO
import Language.Gator.Gates
import Language.Gator.Gates.Trace

import Language.Gator.Ops.General

import qualified Data.Map as M

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
    i <- nextGateID

    let g  = Trace n i
        g' = G_Trace g

    gateSets $ modify (g':)
    return g 

traceToN :: (Out a, In0 b, MonadState Logic m) => Name -> a -> b -> m Trace
traceToN n a b = do
    g <- newTraceN n
    (joints) $ (modify $ js g)
    return g 
    where
        iw = M.insertWith (flip (++))
        js g = (iw (out g) [(in0 b)]) . (iw (out a) [(in0 g)])
