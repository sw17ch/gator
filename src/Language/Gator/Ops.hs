{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Language.Gator.Ops (
    compile,
    connect,
    connect1,
    connectWithIn,

    module Language.Gator.Ops.OR,
    module Language.Gator.Ops.XOR,
    module Language.Gator.Ops.AND,
    module Language.Gator.Ops.Trace,
    module Language.Gator.Ops.Input,
    module Language.Gator.Ops.Output,
) where

import Language.Gator.Logic
import Language.Gator.IO

import Language.Gator.Ops.OR
import Language.Gator.Ops.XOR
import Language.Gator.Ops.AND
import Language.Gator.Ops.Trace
import Language.Gator.Ops.Input
import Language.Gator.Ops.Output

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Error

compile :: (Monad m) => StateT Logic m a -> m Logic
compile g = execStateT g initL

connect :: (Out a, In0 b, MonadState Logic m) => a -> b -> m ()
connect = connectWithIn in0

connect1 :: (Out a, In1 b, MonadState Logic m) => a -> b -> m ()
connect1 = connectWithIn in1

connectWithIn :: (MonadState Logic m, Out a) => (b -> InName) -> a -> b -> m ()
connectWithIn inN a b = do
    (joints) $ (modify $ js)
    where
        js = M.insert (out a) (inN b)
