module AndGate where

import General
import IO

data AndGate = AndGate Name
    deriving (Show,Eq)

instance Named AndGate where
    name (AndGate n) = n

instance Out AndGate
instance In0 AndGate
instance In1 AndGate
