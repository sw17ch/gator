{-# LANGUAGE TemplateHaskell, FlexibleContexts #-} 
module Language.Gator.Logic (
    GateSet,
    inputs, outputs,

    GateIDs,
    orID, xorID, andID,
    traceID, inputID, outputID,

    Logic,
    nextGID, gateSets,
    joints, gateIDs,

    initL,

    mkGr,
) where

import Data.Lenses.Template

import Language.Gator.Gates
import Language.Gator.IO

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import Data.Map (Map)
import qualified Data.Map as M

type Joints = Map OutName [InName]
type GateSet = [Gate]

initGS :: GateSet
initGS = []

inputs :: GateSet -> [Input]
inputs gs = [i | G_Input i <- gs]

outputs :: GateSet -> [Output]
outputs gs = [o | G_Output o <- gs]

data GateIDs = GateIDs {
    orID_     :: Integer,
    xorID_    :: Integer,
    andID_    :: Integer,
    traceID_  :: Integer,
    inputID_  :: Integer,
    outputID_ :: Integer
} deriving (Show)

$( deriveLenses ''GateIDs )

initGI :: GateIDs
initGI = GateIDs 0 0 0 0 0 0

data Logic = Logic {
    nextGID_  :: GateID,
    gateSets_ :: GateSet,
    joints_   :: Joints,
    gateIDs_  :: GateIDs
} deriving (Show)

$( deriveLenses ''Logic )

initL :: Logic
initL = Logic 0 initGS M.empty initGI

mkGr :: Logic -> Gr Name Name
mkGr (Logic _ gs js _) = 
    let lnodes = map swap ns
    in  mkGraph lnodes ledges
    where
        ledges :: [LEdge Name]
        ledges = let es = M.toList js
                 in  map mkLEdge (concatMap xpnd es)

        xpnd :: (OutName,[InName]) -> [(OutName,InName)]
        xpnd (o,is) = zip (repeat o) is

        mkLEdge :: (OutName,InName) -> LEdge Name
        mkLEdge (s1,s2) = let (Just n1) = lookup (outNameToName s1) ns
                              (Just n2) = lookup (inNameToName  s2) ns
                              n = (unOut s1) ++ " -> " ++ (unIn s2)
                          in (n1,n2,n)

        ns = let f g = (gateName g, gateGID g)
             in  map f gs

        swap (a,b) = (b,a)

