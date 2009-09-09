module Language.Gator.IO (
    Named(..),
    Out(..),
    In0(..),
    In1(..),
    OutName,
    InName,
) where

import Language.Gator.General

class Named a where
    name :: a -> Name

class (Named a) => Out a where
    out :: a -> OutName
    out a = OutName $ (name a) ++ ".out"

class (Named a) => In0 a where
    in0 :: a -> InName
    in0 a = InName $ (name a) ++ ".in0"

class (Named a, In0 a) => In1 a where
    in1 :: a -> InName
    in1 a = InName $ (name a) ++ ".in1"

newtype OutName = OutName Name deriving (Show,Ord,Eq)
newtype InName  = InName  Name deriving (Show,Ord,Eq)
