{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Language.Gator.Ops (
    compile,
    newInput,
    newOutput,
    newOr,
    doOr,
    newXOr,
    doXOr,
    newAnd,
    doAnd,
    newTrace,
    lineTo,
    connect,
    connect1,
    connectWithIn,
) where

import Language.Gator.General
import Language.Gator.Logic
import Language.Gator.IO
import Language.Gator.Gates.Input
import Language.Gator.Gates.Output
import Language.Gator.Gates.Trace
import Language.Gator.Gates.AndGate

import Language.Gator.Ops.OR
import Language.Gator.Ops.XOR


import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Error

compile :: (Monad m) => StateT Logic m a -> m Logic
compile g = execStateT g initL

newInput :: (MonadState Logic m) => Name -> m Input
newInput n = do
    (gateSets . inputs) $ (modify $ S.insert g)
    return g
    where
        g = Input n

newOutput :: (MonadState Logic m) => Name -> m Output
newOutput n = do
    (gateSets . outputs) $ (modify $ S.insert g)
    return g
    where
        g = Output n

newAnd :: (MonadState Logic m) => Name -> m AndGate
newAnd n = do
    (gateSets . andGates) $ (modify $ S.insert g)
    return g
    where
        g = AndGate n

doAnd :: (Out a, Out b, MonadState Logic m) => a -> b -> Name -> m AndGate
doAnd a b n = do
    g <- newAnd n
    (joints) $ (modify $ js g)
    return g
    where
        js g = (M.insert (out b) (in1 g)) . (M.insert (out a) (in0 g))

newTrace :: (MonadState Logic m) => Name -> m Trace
newTrace n = do
    (gateSets . traces) $ (modify $ S.insert g)
    return g 
    where
        g = Trace n

lineTo :: (Out a, In0 b, MonadState Logic m) => a -> b -> Name -> m Trace
lineTo a b n = do
    g <- newTrace n
    (joints) $ (modify $ js g)
    return g 
    where
        js g = (M.insert (out g) (in0 b)) . (M.insert (out a) (in0 g))

connect :: (Out a, In0 b, MonadState Logic m) => a -> b -> m ()
connect = connectWithIn in0

connect1 :: (Out a, In1 b, MonadState Logic m) => a -> b -> m ()
connect1 = connectWithIn in1

connectWithIn :: (MonadState Logic m, Out a) => (b -> InName) -> a -> b -> m ()
connectWithIn inN a b = do
    (joints) $ (modify $ js)
    where
        js = M.insert (out a) (inN b)
