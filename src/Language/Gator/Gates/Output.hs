module Language.Gator.Gates.Output where

import Language.Gator.General
import Language.Gator.IO

data Output = Output Name
    deriving (Show,Eq,Ord)

instance Named Output where
    name (Output n) = n

instance In0 Output
