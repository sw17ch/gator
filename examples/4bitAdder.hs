{-# LANGUAGE FlexibleContexts #-}
module Main where

import Language.Gator

main :: IO ()
main = do
    l <- compile logic
    putStrLn "/* "
    print l
    putStrLn " */"
    putStrLn $ mkDot l

logic :: StateT Logic IO ()
logic = do
    inC <- newInputN "inC"

    inA0 <- newInputN "inA0"
    inA1 <- newInputN "inA1"
    inA2 <- newInputN "inA2"
    inA3 <- newInputN "inA3"

    inB0 <- newInputN "inB0"
    inB1 <- newInputN "inB1"
    inB2 <- newInputN "inB2"
    inB3 <- newInputN "inB3"

    out0 <- newOutputN "out0"
    out1 <- newOutputN "out1"
    out2 <- newOutputN "out2"
    out3 <- newOutputN "out3"
    outC <- newOutputN "outC"

    (s0,c0) <- fullAdder inA0 inB0 inC
    (s1,c1) <- fullAdder inA1 inB1 c0
    (s2,c2) <- fullAdder inA2 inB2 c1
    (s3,c3) <- fullAdder inA3 inB3 c2

    connect s0 out0
    connect s1 out1
    connect s2 out2
    connect s3 out3
    connect c3 outC

{-
 - A Full Adder.
 - See: http://en.wikipedia.org/wiki/Adder_(electronics)#Full_adder
 -}
fullAdder :: (Out a, Out b, Out c, MonadState Logic m) => a -> b -> c -> m (XOrGate, OrGate)
fullAdder inA inB inC = do
    xor0 <- doXOr inA inB
    xor1 <- doXOr xor0 inC

    and0 <- doAnd xor0 inC
    and1 <- doAnd inA inB

    or0  <- doOr  and0 and1
        
    return (xor1,or0)
