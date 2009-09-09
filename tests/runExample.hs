module Main where

import Language.Gator.Examples

main :: IO ()
main = do
    putStrLn "Running compileExampleErr"
    compileExampleErr

    putStrLn "Running compileExampleIO"
    compileExampleIO
