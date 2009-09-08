module Input where

import General
import IO

data Input = Input Name
    deriving (Show, Eq, Ord)

instance Named Input where
    name (Input n) = n

instance Out Input
