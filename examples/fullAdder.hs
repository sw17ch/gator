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
    inC <- newInputN "Cin"
    outS <- newOutputN "S"
    outC <- newOutputN "Cout"

    (s,c) <- fullAdder inA inB inC

    connect s outS
    connect c outC

{-
 - A Full Adder.
 - See: http://en.wikipedia.org/wiki/Adder_(electronics)#Full_adder
 -}
fullAdder :: (Out a, Out b, Out c, MonadState Logic m) => a -> b -> c -> m (XOR, OR)
fullAdder inA inB inC = do
    xor0 <- doXOr inA inB
    xor1 <- doXOr xor0 inC

    and0 <- doAnd xor0 inC
    and1 <- doAnd inA inB

    or0  <- doOr  and0 and1
        
    return (xor1,or0)
