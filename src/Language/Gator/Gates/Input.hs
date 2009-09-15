module Language.Gator.Gates.Input where

import Language.Gator.IO

data Input = Input Name GateID
    deriving (Show, Eq, Ord)

instance GIdent Input where
    gid (Input _ g) = g

instance Named Input where
    name (Input n _) = n

instance Out Input
