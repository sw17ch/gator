module Main where

import Language.Gator
import Control.Monad.State

main :: IO ()
main = do
    l <- compile halfAdder
    print l

{-
 - A Half Adder.
 - See: http://en.wikipedia.org/wiki/Adder_(electronics)#Half_adder
 -}
halfAdder :: StateT Logic IO ()
halfAdder = do
    inA <- newInput "A"
    inB <- newInput "B"
    outS <- newOutput "S"
    outC <- newOutput "C"

    xor <- doXOr inA inB "xor0"
    and <- doAnd inA inB "and0"

    connect xor outS
    connect and outC
