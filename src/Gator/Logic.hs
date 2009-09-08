module Logic where

import General
import Gates

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

type Joints = Map Name Name

data GateSets = GateSets {
    orGates :: Set OrGate,
    andGates :: Set AndGate,
    traces :: Set Line,
    inputs :: Set Input,
    outputs :: Set Output
} deriving (Show)

initGS :: GateSets
initGS = GateSets S.empty S.empty S.empty S.empty S.empty 

data Logic = Logic {
    gateSets :: GateSets,
    joints   :: Joints
} deriving (Show)

initL = Logic initGS M.empty
