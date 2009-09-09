{-# LANGUAGE FlexibleContexts #-}
module Language.Gator.Ops (
    compile,
    newInput,
    newOutput,
    newOr,
    doOr,
    newLine,
    lineTo,
) where

import Language.Gator.General
import Language.Gator.Logic
import Language.Gator.IO

import Language.Gator.Gates.Input
import Language.Gator.Gates.Output
import Language.Gator.Gates.Line
import Language.Gator.Gates.AndGate
import Language.Gator.Gates.OrGate

import Data.Lenses

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.State
import Control.Monad.Error

compile :: (Monad m) => StateT Logic m a -> m Logic
compile g = execStateT g initL

newInput :: (MonadState Logic m) => Name -> m Input
newInput n = do
    s <- get

    let ins = s `fetch` l
        s'  = s `update` l $ S.insert g ins

    put s'

    return g

    where g = Input n
          l = gateSets . inputs


newOutput :: (MonadState Logic m) => Name -> m Output
newOutput n = do
    s <- get

    let outs  = s `fetch` l
        s'    = s `update` l $ S.insert g outs

    put s'

    return g

    where g = Output n
          l = gateSets . outputs

newOr :: (MonadState Logic m) => Name -> m OrGate
newOr n = do
    s <- get

    let ors  = s `fetch` l
        s'   = s `update` l $ S.insert g ors

    put s'

    return g

    where g = OrGate n
          l = gateSets . orGates

doOr :: (Out a, Out b, MonadState Logic m) => a -> b -> Name -> m OrGate
doOr a b n = do
    g <- newOr n
    s <- get

    let js  = s `fetch` l
        js' = M.insert (out b) (in1 g) $ M.insert (out a) (in0 g) js
        s'  = s `update` l $ js'
    
    put s'

    return g
    where l = joints

newLine :: (MonadState Logic m) => Name -> m Line
newLine n = do
    s <- get

    let lns = s `fetch` l
        s'  = s `update` l $ S.insert ln lns 

    put s'

    return ln

    where ln = Line n
          l = gateSets . traces

lineTo :: (Out a, In0 b, MonadState Logic m) => a -> b -> Name -> m Line
lineTo a b n = do
    ln <- newLine n
    s <- get

    let js  = s `fetch` l
        js' = M.insert (out ln) (in0 b) $ M.insert (out a) (in0 ln) js
        s'  = s `update` l $ js'

    put s'

    return ln

    where l = joints
