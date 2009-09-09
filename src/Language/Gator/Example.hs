{-# LANGUAGE FlexibleContexts #-}
module Language.Gator.Example where

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

compileExampleIO :: IO ()
compileExampleIO = do
    l <- compile gatesIO
    print l

gatesIO :: StateT Logic IO ()
gatesIO = do
    in0  <- newInput "in0"
    in1  <- newInput "in1"
    out0 <- newOutput "out0"

    lift $ print (in0,in1,out0)

    o <- doOr in0 in1 "or0"
    lineTo o out0 "ln0"
    return ()


    {- Produces:
     -  Logic {
     -      gateSets = GateSets {
     -          orGates = fromList [OrGate "or0"],
     -          andGates = fromList [],
     -          traces = fromList [Line "ln0"],
     -          inputs = fromList [Input "in0",Input "in1"],
     -          outputs = fromList [Output "out0"]
     -      },
     -      joints = fromList [
     -          ("in0.out","or0.in0"),
     -          ("in1.out","or0.in1"),
     -          ("or0.out","out0.in0")
     -      ]
     -  }
     -}
