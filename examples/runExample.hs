module Main where

import Language.Gator.Examples.IO
import Language.Gator.Examples.Err


main :: IO ()
main = do
    putStrLn "Running compileExampleErr"
    compileExampleErr

    putStrLn "Running compileExampleIO"
    compileExampleIO
