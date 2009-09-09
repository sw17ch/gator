{-# LANGUAGE FlexibleContexts #-}
module Language.Gator.Examples.IO where

import Language.Gator.Ops
import Language.Gator.Logic

import Control.Monad.State
import Control.Monad.Error

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

