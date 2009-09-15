module Language.Gator.Gates.AND where

import Language.Gator.IO

data AND = AND Name GateID
    deriving (Show,Ord,Eq)

instance GIdent AND where
    gid (AND _ g) = g

instance Named AND where
    name (AND n _) = n

instance Out AND
instance In0 AND
instance In1 AND
