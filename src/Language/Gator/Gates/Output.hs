module Language.Gator.Gates.Output where

import Language.Gator.IO

data Output = Output Name GateID
    deriving (Show,Eq,Ord)

instance GIdent Output where
    gid (Output _ g) = g

instance Named Output where
    name (Output n _) = n

instance In0 Output
