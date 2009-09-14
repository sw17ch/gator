module Language.Gator.Gates.OR where

import Language.Gator.IO

data OR = OR Name
    deriving (Show,Eq,Ord)

instance Named OR where
    name (OR n) = n

instance Out OR
instance In0 OR
instance In1 OR
