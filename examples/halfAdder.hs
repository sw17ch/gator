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
    inA <- newInputN "A"
    inB <- newInputN "B"
    outS <- newOutputN "S"
    outC <- newOutputN "C"

    (xor,and) <- halfAdder inA inB

    connect xor outS
    connect and outC

{-
 - A Half Adder.
 - See: http://en.wikipedia.org/wiki/Adder_(electronics)#Half_adder
 -}
halfAdder :: (Out a, Out b, MonadState Logic m) => a -> b -> m (XOR, AND)
halfAdder inA inB = do
    xor <- doXOr inA inB
    and <- doAnd inA inB
    return (xor,and)
