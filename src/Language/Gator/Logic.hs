{-# LANGUAGE TemplateHaskell, FlexibleContexts #-} 
module Language.Gator.Logic (
    GateSet,
    {-
    GateSets,
    orGates, xorGates, andGates,
    traces, inputs, outputs,
    -}

    GateIDs,
    orID, xorID, andID,
    traceID, inputID, outputID,

    Logic,
    gateSets, joints, gateIDs,

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

{-
data GateSets = GateSets {
    orGates_  :: Set OR,
    xorGates_ :: Set XOR,
    andGates_ :: Set AND,
    traces_   :: Set Trace,
    inputs_   :: Set Input,
    outputs_  :: Set Output
} deriving (Show)

$( deriveLenses ''GateSets )

initGS :: GateSets
initGS = GateSets S.empty S.empty S.empty
                  S.empty S.empty S.empty
-}

type GateSet = [Gate]

initGS :: GateSet
initGS = []

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
    gateSets_ :: GateSet,
    joints_   :: Joints,
    gateIDs_  :: GateIDs
} deriving (Show)

$( deriveLenses ''Logic )

initL :: Logic
initL = Logic initGS M.empty initGI

mkGr :: Logic -> Gr Name Name
mkGr (Logic gs js _) = 
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

        ns :: [(Name,Node)]
        ns = zip names [1..]

        names = map gateName gs

        swap (a,b) = (b,a)


