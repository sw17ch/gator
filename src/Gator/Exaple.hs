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
    in0  <- newInput "in0"
    in1  <- newInput "in1"
    out0 <- newOutput "out0"

    o <- doOr in0 in1 "or0"
    lineTo o out0 "ln0"

    {- Produces:
     -  Logic {
     -      gateSets = GateSets {
     -          orGates = fromList [OrGate "or0"],
     -          andGates = fromList [],
     -          traces = fromList [Line "ln0"],
     -          inputs = fromList [Input "in0",Input "in1"],
     -          outputs = fromList [Output "out0"]
     -      },
     -      joints = fromList [
     -          ("in0.out","or0.in0"),
     -          ("in1.out","or0.in1"),
     -          ("or0.out","out0.in0")
     -      ]
     -  }
     -}