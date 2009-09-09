module Language.Gator.IO (
    Named(..),
    Out(..),
    In0(..),
    In1(..),
) where

class Named a where
    name :: a -> String

class (Named a) => Out a where
    out :: a -> String
    out a = (name a) ++ ".out"

class (Named a) => In0 a where
    in0 :: a -> String
    in0 a = (name a) ++ ".in0"

class (Named a, In0 a) => In1 a where
    in1 :: a -> String
    in1 a = (name a) ++ ".in1"
