module Language.Gator.Gates (
    OR,
    XOR,
    AND,
    Trace,
    Input,
    Output,

    Gate(..),
    gateName,
    gateGID,
) where

import Language.Gator.Gates.OR
import Language.Gator.Gates.XOR
import Language.Gator.Gates.AND
import Language.Gator.Gates.Trace
import Language.Gator.Gates.Input
import Language.Gator.Gates.Output

import Language.Gator.IO

data Gate = G_OR     OR
          | G_XOR    XOR
          | G_AND    AND
          | G_Trace  Trace
          | G_Input  Input
          | G_Output Output
    deriving (Show)

gateName :: Gate -> Name
gateName (G_OR g) = name g
gateName (G_XOR g) = name g
gateName (G_AND g) = name g
gateName (G_Trace g) = name g
gateName (G_Input g) = name g
gateName (G_Output g) = name g

gateGID :: Gate -> GateID
gateGID (G_OR g) = gid g
gateGID (G_XOR g) = gid g
gateGID (G_AND g) = gid g
gateGID (G_Trace g) = gid g
gateGID (G_Input g) = gid g
gateGID (G_Output g) = gid g
