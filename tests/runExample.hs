module Main where

import Language.Gator.Example

main :: IO ()
main = do
    putStrLn "Running compileExampleErr"
    compileExampleErr

    putStrLn "Running compileExampleIO"
    compileExampleIO
