{-# LANGUAGE RankNTypes #-}
module Main where

import IO
import General
import Gates
import Logic

import Ops


main :: IO ()
main = do
    print $ compile gates

gates = do
    o <- doOr in0 in1 "or0"
    lineTo o out0 "ln0"
    where
        in0  = Input "in0"
        in1  = Input "in1"
        out0 = Output "out0"

    {- Produces:
     -  Logic {gateSets = GateSets {
     -          orGates = fromList [OrGate "or0"],
     -          andGates = fromList [],
     -          traces = fromList [Line "ln0"],
     -          inputs = fromList [],
     -          outputs = fromList []
     -      },
     -      joints = fromList [
     -          ("in0.out","or0.in0"),
     -          ("in1.out","or0.in1"),
     -          ("or0.out","out0.in0")
     -      ]
     -  }
     -}
