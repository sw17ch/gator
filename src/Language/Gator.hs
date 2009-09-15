module Language.Gator (
    module Language.Gator.General,
    module Language.Gator.IO,
    module Language.Gator.Logic,
    module Language.Gator.Ops,
    module Language.Gator.Gates,

    module Control.Monad.State,

    mkDot,
) where

import Data.Lenses
import Data.GraphViz
import Data.GraphViz.Types

-- All below here is exported
import Language.Gator.General
import Language.Gator.IO (
        Named(..), GIdent(..), Out(..),
        In0(..), In1(..),
        OutName, InName
    )
import Language.Gator.Logic
import Language.Gator.Ops
import Language.Gator.Gates
import Control.Monad.State

mkDot :: Logic -> String
mkDot l = let g = mkGr l
              dot = graphToDot True g [] nods edgs
              stmts = graphStatements dot
              sgs = subGraphs stmts
              sgs' = osg : isg : sgs
              stmts' = stmts { subGraphs = sgs' }
              dot' = dot { graphStatements = stmts' }
          in printDotGraph dot'
    where edgs (_,_,_) = [] -- [Label $ StrLabel $ n]
          nods (_,n) = [Label $ StrLabel $ n]

          inputNodes = let mkN i = DotNode i []
                       in map mkN $ map gid $ inputs $ (l `fetch` gateSets)

          outputNodes = let mkN i = DotNode i []
                        in map mkN $ map gid $ outputs $ (l `fetch` gateSets)

          isg = DotSG {
            isCluster = False,
            subGraphID = Just $ Str "inputs",
            subGraphStmts = DotStmts {
                attrStmts = [GraphAttrs [Rank $ SameRank]],
                subGraphs = [],
                nodeStmts = inputNodes,
                edgeStmts = []
            }
          }

          osg = DotSG {
            isCluster = False,
            subGraphID = Just $ Str "outputs",
            subGraphStmts = DotStmts {
                attrStmts = [GraphAttrs [Rank $ SameRank]],
                subGraphs = [],
                nodeStmts = outputNodes,
                edgeStmts = []
            }
          }
