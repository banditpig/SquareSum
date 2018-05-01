{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
import           Control.Monad
import qualified Data.GraphViz                     as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types               as G
import           Data.List
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.IO                 as TL
import           System.Exit
import           System.Process

type Label = Int
type V = [(Int, Label)]
type E = [(Int,Int, Label)]
type Graph = (V, E)

defaultLabel :: Label
defaultLabel = 0

sameEdge :: (Int, Int, Int) ->  (Int, Int, Int) -> Bool
sameEdge (a, b, _) (c, d, _) = a == d && b == c || a == c && b == d

buildGraph ns = (vs, es) where
    vs = zip ns . repeat $ defaultLabel
    es = nubBy sameEdge . join . makeEdges $ ns
    makeEdges  = map f where
        f n = foldr (\v a -> if n /= v && isSquare (n + v) then (n, v, defaultLabel) : a else a) [] ns

graphParams :: G.GraphvizParams Int Int Int () Int
graphParams = G.defaultParams {
  G.fmtNode = const [colorAttribute $ G.RGB 0 0 0],
  G.fmtEdge = const [G.arrowTo G.noArrow, colorAttribute (G.RGB 200 0 0)]}
  where
    colorAttribute color = G.Color $ G.toColorList [ color ]

isSquare :: Integral a => a -> Bool
isSquare n = sq * sq == n where sq = floor $ sqrt (fromIntegral n :: Double)

main :: IO ()
main = do
    let (vs, es) = buildGraph [1..24]
    let dotGraph = G.graphElemsToDot graphParams vs es :: G.DotGraph Int
    let dotText  = G.printDotGraph dotGraph :: TL.Text
    TL.writeFile "numbers.dot" dotText
    callCommand  "dot numbers.dot -Tpng > numbers.png" >>= print
