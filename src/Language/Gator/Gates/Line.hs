module Line where

import General
import IO

data Line = Line Name
    deriving (Show,Eq,Ord)

instance Named Line where
    name (Line n) = n

instance Out Line
instance In0 Line
