module Language.Gator.Gates.Trace where

import Language.Gator.General
import Language.Gator.IO

data Trace = Trace Name
    deriving (Show,Eq,Ord)

instance Named Trace where
    name (Trace n) = n

instance Out Trace
instance In0 Trace
