module Language.Gator.Gates.Input where

import Language.Gator.IO

data Input = Input Name
    deriving (Show, Eq, Ord)

instance Named Input where
    name (Input n) = n

instance Out Input
