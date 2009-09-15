module Language.Gator.Gates.OR where

import Language.Gator.IO

data OR = OR Name GateID
    deriving (Show,Eq,Ord)

instance GIdent OR where
    gid (OR _ g) = g

instance Named OR where
    name (OR n _) = n

instance Out OR
instance In0 OR
instance In1 OR
