module Output where

import General
import IO

data Output = Output Name
    deriving (Show,Eq,Ord)

instance Named Output where
    name (Output n) = n

instance In0 Output
