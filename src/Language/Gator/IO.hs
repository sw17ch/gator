module Language.Gator.IO (
    Named(..),
    GIdent(..),
    Out(..),
    In0(..),
    In1(..),
    OutName(..),
    InName(..),
    outNameToName,
    inNameToName,
    Name,
    GateID,
) where

import Language.Gator.General

type GateID = Int

class Named a where
    name :: a -> Name

class GIdent a where
    gid :: a -> GateID

class (Named a) => Out a where
    out :: a -> OutName
    out a = OutName $ (name a) ++ ".out"

class (Named a) => In0 a where
    in0 :: a -> InName
    in0 a = InName $ (name a) ++ ".in0"

class (Named a, In0 a) => In1 a where
    in1 :: a -> InName
    in1 a = InName $ (name a) ++ ".in1"

newtype OutName = OutName { unOut :: Name } deriving (Show,Ord,Eq)
newtype InName  = InName  { unIn  :: Name } deriving (Show,Ord,Eq)

outNameToName :: OutName -> Name
outNameToName (OutName n) = takeWhile (/= '.') n

inNameToName  :: InName  -> Name
inNameToName (InName n)  = takeWhile (/= '.') n
