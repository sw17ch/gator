module Language.Gator (
    module Language.Gator.General,
    module Language.Gator.IO,
    module Language.Gator.Logic,
    module Language.Gator.Ops,
    module Language.Gator.Gates,

    module Control.Monad.State,

    mkDot,
) where

import Data.GraphViz
import Data.GraphViz.Types
import System.IO.Unsafe

-- All below here is exported
import Language.Gator.General
import Language.Gator.IO (
        Named(..), Out(..),
        In0(..), In1(..),
        OutName, InName
    )
import Language.Gator.Logic
import Language.Gator.Ops
import Language.Gator.Gates
import Control.Monad.State

mkDot :: Logic -> String
mkDot l = let g = mkGr l
              dot = graphToDot False g [] nods edgs
              x = unsafePerformIO $ print g
          in x `seq` printDotGraph dot 
    where edgs (_,_,_) = [] -- [Label $ StrLabel $ n]
          nods (_,n) = [Label $ StrLabel $ n]
