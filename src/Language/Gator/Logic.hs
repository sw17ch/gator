{-# LANGUAGE TemplateHaskell, FlexibleContexts #-} 
module Language.Gator.Logic where

import Data.Lenses.Template

import Language.Gator.General
import Language.Gator.Gates

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

type Joints = Map Name Name

data GateSets = GateSets {
    orGates_  :: Set OrGate,
    andGates_ :: Set AndGate,
    traces_   :: Set Line,
    inputs_   :: Set Input,
    outputs_  :: Set Output
} deriving (Show)

$( deriveLenses ''GateSets )

initGS :: GateSets
initGS = GateSets S.empty S.empty S.empty S.empty S.empty 

data Logic = Logic {
    gateSets_ :: GateSets,
    joints_   :: Joints
} deriving (Show)

$( deriveLenses ''Logic )

initL :: Logic
initL = Logic initGS M.empty
