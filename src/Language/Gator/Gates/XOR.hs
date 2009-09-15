module Language.Gator.Gates.XOR (
    XOR(..),
) where

import Language.Gator.IO

data XOR = XOR Name GateID
    deriving (Show,Ord,Eq)

instance GIdent XOR where
    gid (XOR _ g) = g

instance Named XOR where
    name (XOR n _) = n

instance Out XOR 
instance In0 XOR 
instance In1 XOR 
