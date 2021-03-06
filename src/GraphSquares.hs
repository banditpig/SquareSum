{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Control.Monad
import qualified Data.GraphViz                     as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types               as G
import           Data.List
import           Data.Monoid
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.IO                 as TL
import           System.Exit
import           System.Process

newtype V a     = V [a]        deriving Show
newtype E a     = E [(a, a)]   deriving Show
newtype Graph a = G (V a ,E a) deriving Show


buildGraph :: (Eq a) => [a] -> (a -> a -> Bool) -> Graph a
buildGraph vs ed = G (V vs, E es) where
    es = nubBy sameEdge . join . makeEdges $ vs
    makeEdges  = map f where
        f n = foldr (\v a -> if ed n v then (n, v) : a else a) [] vs

sameEdge ::(Eq a) => (a, a) ->  (a, a) -> Bool
sameEdge (a, b) (c, d) = a == d && b == c || a == c && b == d

sqEdge :: (Integral a, Num a) => a -> a -> Bool
sqEdge n v =  n /= v && isSquare (n + v)

isSquare :: Integral a => a -> Bool
isSquare n = sq * sq == n where sq = floor $ sqrt (fromIntegral n :: Double)



-- Labelling of vertices and edges for use in Graphviz
graphLabeller :: (a -> b) -> (a -> a -> c) -> Graph a -> ([(a,b)], [(a,a,c)])
graphLabeller vf ef (G (V vs, E es)) = (vs', es') where
    vs' = map (\ v -> (v, vf v)) vs
    es' = map (\ (x, y) -> (x, y, ef x y)) es

graphParams ::(Show a) => G.GraphvizParams a a a () a
graphParams = G.defaultParams {
  G.fmtNode = const [colorAttribute $ G.RGB 0 0 0],
  G.fmtEdge = \(t, f, l) -> [G.textLabel (TL.pack $ show l), G.arrowTo G.noArrow, colorAttribute (G.RGB 200 0 0)]}
  where
    colorAttribute color = G.Color $ G.toColorList [ color ]


makeImage :: FilePath -> ([(Int, Int)], [(Int, Int, Int)]) -> IO ()
makeImage name (vs, es) = do
    let dotGraph = G.graphElemsToDot graphParams vs es :: G.DotGraph Int
    let dotText  = G.printDotGraph dotGraph :: TL.Text
    TL.writeFile (name <> ".dot") dotText
    callCommand  ("dot " <> name <> ".dot -Tpng > " <> name <> ".png") >>= print

graphToImage :: Int -> IO ()
graphToImage  n = do
    let g = buildGraph [1..n] sqEdge
    let (vs, es) = graphLabeller id (+) g
    makeImage ("../images/1_" <> show n) (vs, es)

main :: IO ()
main = do

    let g = buildGraph [1..15] sqEdge
    let (vs, es) = graphLabeller id (+) g
    makeImage "../images/1_15" (vs, es)

    let g = buildGraph [1..16] sqEdge
    let (vs, es) = graphLabeller id (+) g
    makeImage "../images/1_16" (vs, es)

    let g = buildGraph [1..17 ] sqEdge
    let (vs, es) = graphLabeller id (+) g
    makeImage "../images/1_17" (vs, es)

    let g = buildGraph [1..50 ] sqEdge
    let (vs, es) = graphLabeller id (+) g
    makeImage "../images/1_50" (vs, es)


--  8 1 15 10 6 3 13 12 4 5 11 14 2 7 9
