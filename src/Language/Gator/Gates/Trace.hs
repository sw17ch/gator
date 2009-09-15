module Language.Gator.Gates.Trace where

import Language.Gator.IO

data Trace = Trace Name GateID
    deriving (Show,Eq,Ord)

instance GIdent Trace where
    gid (Trace _ g) = g

instance Named Trace where
    name (Trace n _) = n

instance Out Trace
instance In0 Trace
