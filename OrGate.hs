module OrGate where

import General
import IO

data OrGate = OrGate Name
    deriving (Show,Eq,Ord)

instance Named OrGate where
    name (OrGate n) = n

instance Out OrGate
instance In0 OrGate
instance In1 OrGate
