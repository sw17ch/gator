module Language.Gator.Gates.XOR (
    XOR(..),
) where

import Language.Gator.IO

data XOR = XOR Name
    deriving (Show,Ord,Eq)

instance Named XOR where
    name (XOR n) = n

instance Out XOR 
instance In0 XOR 
instance In1 XOR 
