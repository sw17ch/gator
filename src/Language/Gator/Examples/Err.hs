{-# LANGUAGE FlexibleContexts #-}
module Language.Gator.Examples.Err where

import Language.Gator.Ops
import Language.Gator.Logic

import Control.Monad.State
import Control.Monad.Error

compileExampleErr :: IO ()
compileExampleErr = do
    case compile gatesErr of
        (Left err) -> putStrLn $ "Error: " ++ err
        (Right l) -> print l

gatesErr :: (MonadError String m, MonadState Logic m) => m ()
gatesErr = do
    in0  <- newInput "in0"
    out0 <- newOutput "out0"

    throwError "Whoops"

    lineTo in0 out0 "ln0"
    return ()

