module Language.Gator.Gates.AND where

import Language.Gator.IO

data AND = AND Name
    deriving (Show,Ord,Eq)

instance Named AND where
    name (AND n) = n

instance Out AND
instance In0 AND
instance In1 AND
