module Language.Gator.Gates (
    OR,
    XOR,
    AND,
    Trace,
    Input,
    Output,

    Gate(..),
) where

import Language.Gator.Gates.OR
import Language.Gator.Gates.XOR
import Language.Gator.Gates.AND
import Language.Gator.Gates.Trace
import Language.Gator.Gates.Input
import Language.Gator.Gates.Output

data Gate = G_OR     OR
          | G_XOR    XOR
          | G_AND    AND
          | G_Trace  Trace
          | G_Input  Input
          | G_Output Output
    deriving (Show)
