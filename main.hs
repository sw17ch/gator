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
    newOr "or10"
    o <- doOr in0 in1 "or0"
    lineTo o out0 "ln0"
    where
        in0  = Input "in0"
        in1  = Input "in1"
        out0 = Output "out0"
