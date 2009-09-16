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
              dot = graphToDot True g [gas] nods edgs
              stmts = graphStatements dot
              sgs = subGraphs stmts
              sgs' = (mkDotSG "outputs" outputNodes) :
                     (mkDotSG "inputs" inputNodes) :
                     sgs
              stmts' = stmts { subGraphs = sgs' }
              dot' = dot { graphStatements = stmts' }
          in printDotGraph dot'
    where gas = GraphAttrs [ RankDir FromLeft ]
          edgs (_,_,_) = []
          nods (_,n) = [Label $ StrLabel $ n]

          (inputNodes,outputNodes) = let mkN i = DotNode i []
                                         g f = (map mkN) . (map gid) . f $ (l `fetch` gateSets)
                                     in (g inputs, g outputs)

          mkDotSG sgName nodes = DotSG {
            isCluster = False,
            subGraphID = Just $ Str sgName,
            subGraphStmts = DotStmts {
                attrStmts = [GraphAttrs [Rank $ SameRank]],
                subGraphs = [],
                nodeStmts = nodes,
                edgeStmts = []
            }
          }
