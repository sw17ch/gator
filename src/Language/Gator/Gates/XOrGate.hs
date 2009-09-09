module Language.Gator.Gates.XOrGate (
    XOrGate(..),
) where

import Language.Gator.General
import Language.Gator.IO

data XOrGate = XOrGate Name
    deriving (Show,Ord,Eq)

instance Named XOrGate where
    name (XOrGate n) = n

instance Out XOrGate 
instance In0 XOrGate 
instance In1 XOrGate 
