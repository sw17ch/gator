module Language.Gator.Gates.AndGate where

import Language.Gator.General
import Language.Gator.IO

data AndGate = AndGate Name
    deriving (Show,Ord,Eq)

instance Named AndGate where
    name (AndGate n) = n

instance Out AndGate
instance In0 AndGate
instance In1 AndGate
