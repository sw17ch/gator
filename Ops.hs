module Ops where

import Logic
import General
import Gates
import IO

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad.State

compile g = execState g initL

newOr n = do
    s <- get

    let sets  = gateSets s
        ors   = orGates sets
        newo  = S.insert g ors
        news  = sets { orGates = newo }

    put $ s { gateSets = news }

    return g

    where g = OrGate n

doOr a b n = do
    g <- newOr n
    s <- get

    let js   = joints s
        new1 = M.insert (out a) (in0 g) js
        new2 = M.insert (out b) (in1 g) new1
    
    put $ s { joints = new2 }

    return g

newLine n = do
    s <- get

    let sets = gateSets s
        lns  = traces sets
        newl = S.insert l lns
        news = sets { traces = newl }

    put $ s { gateSets = news }

    return l

    where l = Line n

lineTo a b n = do
    l <- newLine n
    s <- get

    let js    = joints s
        new   = M.insert (out a) (in0 b) js

    put $ s { joints = new }

    return l
