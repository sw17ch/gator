{-# LANGUAGE FlexibleContexts #-}
module Language.Gator.Ops where

import Language.Gator.General
import Language.Gator.Logic
import Language.Gator.Gates
import Language.Gator.IO

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.State
import Control.Monad.Error

--compile :: State Logic a -> Logic
-- evalStateT :: Monad m => StateT s m a -> s -> m a
-- compile :: StateT Logic (Either String) a -> Either String Logic
compile :: (Monad m) => StateT Logic m a -> m Logic
compile g = execStateT g initL

newInput :: (MonadState Logic m) => Name -> m Input
newInput n = do
    s <- get

    let sets = gateSets s
        ins  = inputs sets
        newi = S.insert g ins
        news = sets { inputs = newi }

    put $ s { gateSets = news }

    return g

    where g = Input n


newOutput :: (MonadState Logic m) => Name -> m Output
newOutput n = do
    s <- get

    let sets = gateSets s
        outs = outputs sets
        newo = S.insert g outs
        news = sets { outputs = newo }

    put $ s { gateSets = news }

    return g

    where g = Output n

newOr :: (MonadState Logic m) => Name -> m OrGate
newOr n = do
    s <- get

    let sets  = gateSets s
        ors   = orGates sets
        newo  = S.insert g ors
        news  = sets { orGates = newo }

    put $ s { gateSets = news }

    return g

    where g = OrGate n

doOr :: (Out a, Out b, MonadState Logic m) => a -> b -> Name -> m OrGate
doOr a b n = do
    g <- newOr n
    s <- get

    let js   = joints s
        new1 = M.insert (out a) (in0 g) js
        new2 = M.insert (out b) (in1 g) new1
    
    put $ s { joints = new2 }

    return g

newLine :: (MonadState Logic m) => Name -> m Line
newLine n = do
    s <- get

    let sets = gateSets s
        lns  = traces sets
        newl = S.insert l lns
        news = sets { traces = newl }

    put $ s { gateSets = news }

    return l

    where l = Line n

lineTo :: (Out a, In0 b, MonadState Logic m) => a -> b -> Name -> m Line
lineTo a b n = do
    l <- newLine n
    s <- get

    let js    = joints s
        new   = M.insert (out a) (in0 b) js

    put $ s { joints = new }

    return l
