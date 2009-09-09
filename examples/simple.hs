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
    outC <- newOutputN "C"

    o <- doOr inA inB

    connect o outC

