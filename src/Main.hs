{-# LANGUAGE DisambiguateRecordFields #-}
module Main where

import Slither
import Printer
import Solver
import Data.Array
import Data.Map (fromList)
import Control.Concurrent (threadDelay)

s = makeSlither (7, 7) [ ((0, 0), 2)
                       , ((0, 1), 2)
                       , ((0, 2), 2)
                       , ((0, 3), 3)
                       , ((0, 4), 3)
                       , ((0, 5), 1)
                       , ((1, 2), 0)
                       , ((1, 3), 2)
                       , ((1, 5), 3)
                       , ((3, 0), 3)
                       , ((3, 1), 3)
                       , ((3, 4), 3)
                       , ((3, 5), 3)
                       , ((4, 2), 2)
                       , ((4, 4), 2)
                       , ((5, 0), 3)
                       , ((5, 2), 2)
                       , ((5, 4), 2)
                       , ((5, 5), 2)
                       , ((6, 0), 3)
                       , ((6, 2), 2)
                       , ((6, 5), 3)
                       --
                       , ((4, 6), 2)
                       , ((6, 6), 3)
                       ]

bads = Slither {size = (7,7), lines = fromList [(Line ((0,0),(0,1)),Nothing),(Line ((0,0),(1,0)),Nothing),(Line ((0,1),(0,2)),Nothing),(Line ((0,1),(1,1)),Nothing),(Line ((0,2),(0,3)),Nothing),(Line ((0,2),(1,2)),Nothing),(Line ((0,3),(0,4)),Nothing),(Line ((0,3),(1,3)),Nothing),(Line ((0,4),(0,5)),Nothing),(Line ((0,4),(1,4)),Just L),(Line ((0,5),(0,6)),Nothing),(Line ((0,5),(1,5)),Nothing),(Line ((0,6),(0,7)),Nothing),(Line ((0,6),(1,6)),Nothing),(Line ((0,7),(1,7)),Nothing),(Line ((1,0),(1,1)),Nothing),(Line ((1,0),(2,0)),Nothing),(Line ((1,1),(1,2)),Nothing),(Line ((1,1),(2,1)),Nothing),(Line ((1,2),(1,3)),Just X),(Line ((1,2),(2,2)),Just X),(Line ((1,3),(1,4)),Nothing),(Line ((1,3),(2,3)),Just X),(Line ((1,4),(1,5)),Nothing),(Line ((1,4),(2,4)),Nothing),(Line ((1,5),(1,6)),Nothing),(Line ((1,5),(2,5)),Nothing),(Line ((1,6),(1,7)),Nothing),(Line ((1,6),(2,6)),Nothing),(Line ((1,7),(2,7)),Nothing),(Line ((2,0),(2,1)),Nothing),(Line ((2,0),(3,0)),Nothing),(Line ((2,1),(2,2)),Nothing),(Line ((2,1),(3,1)),Nothing),(Line ((2,2),(2,3)),Just X),(Line ((2,2),(3,2)),Nothing),(Line ((2,3),(2,4)),Nothing),(Line ((2,3),(3,3)),Nothing),(Line ((2,4),(2,5)),Nothing),(Line ((2,4),(3,4)),Nothing),(Line ((2,5),(2,6)),Nothing),(Line ((2,5),(3,5)),Nothing),(Line ((2,6),(2,7)),Nothing),(Line ((2,6),(3,6)),Nothing),(Line ((2,7),(3,7)),Nothing),(Line ((3,0),(3,1)),Nothing),(Line ((3,0),(4,0)),Nothing),(Line ((3,1),(3,2)),Nothing),(Line ((3,1),(4,1)),Just L),(Line ((3,2),(3,3)),Nothing),(Line ((3,2),(4,2)),Nothing),(Line ((3,3),(3,4)),Nothing),(Line ((3,3),(4,3)),Nothing),(Line ((3,4),(3,5)),Nothing),(Line ((3,4),(4,4)),Nothing),(Line ((3,5),(3,6)),Nothing),(Line ((3,5),(4,5)),Just L),(Line ((3,6),(3,7)),Nothing),(Line ((3,6),(4,6)),Nothing),(Line ((3,7),(4,7)),Nothing),(Line ((4,0),(4,1)),Nothing),(Line ((4,0),(5,0)),Nothing),(Line ((4,1),(4,2)),Nothing),(Line ((4,1),(5,1)),Nothing),(Line ((4,2),(4,3)),Nothing),(Line ((4,2),(5,2)),Nothing),(Line ((4,3),(4,4)),Nothing),(Line ((4,3),(5,3)),Nothing),(Line ((4,4),(4,5)),Nothing),(Line ((4,4),(5,4)),Nothing),(Line ((4,5),(4,6)),Nothing),(Line ((4,5),(5,5)),Nothing),(Line ((4,6),(4,7)),Nothing),(Line ((4,6),(5,6)),Nothing),(Line ((4,7),(5,7)),Nothing),(Line ((5,0),(5,1)),Nothing),(Line ((5,0),(6,0)),Nothing),(Line ((5,1),(5,2)),Nothing),(Line ((5,1),(6,1)),Nothing),(Line ((5,2),(5,3)),Nothing),(Line ((5,2),(6,2)),Nothing),(Line ((5,3),(5,4)),Nothing),(Line ((5,3),(6,3)),Nothing),(Line ((5,4),(5,5)),Nothing),(Line ((5,4),(6,4)),Nothing),(Line ((5,5),(5,6)),Nothing),(Line ((5,5),(6,5)),Nothing),(Line ((5,6),(5,7)),Nothing),(Line ((5,6),(6,6)),Nothing),(Line ((5,7),(6,7)),Nothing),(Line ((6,0),(6,1)),Just L),(Line ((6,0),(7,0)),Nothing),(Line ((6,1),(6,2)),Nothing),(Line ((6,1),(7,1)),Nothing),(Line ((6,2),(6,3)),Nothing),(Line ((6,2),(7,2)),Nothing),(Line ((6,3),(6,4)),Nothing),(Line ((6,3),(7,3)),Nothing),(Line ((6,4),(6,5)),Nothing),(Line ((6,4),(7,4)),Nothing),(Line ((6,5),(6,6)),Nothing),(Line ((6,5),(7,5)),Nothing),(Line ((6,6),(6,7)),Nothing),(Line ((6,6),(7,6)),Nothing),(Line ((6,7),(7,7)),Nothing),(Line ((7,0),(7,1)),Nothing),(Line ((7,1),(7,2)),Nothing),(Line ((7,2),(7,3)),Nothing),(Line ((7,3),(7,4)),Nothing),(Line ((7,4),(7,5)),Nothing),(Line ((7,5),(7,6)),Nothing),(Line ((7,6),(7,7)),Nothing)], boxes = array ((0,0),(6,6)) [((0,0),Nothing),((0,1),Just Blue),((0,2),Nothing),((0,3),Nothing),((0,4),Nothing),((0,5),Nothing),((0,6),Nothing),((1,0),Just Blue),((1,1),Nothing),((1,2),Nothing),((1,3),Nothing),((1,4),Nothing),((1,5),Nothing),((1,6),Nothing),((2,0),Nothing),((2,1),Nothing),((2,2),Nothing),((2,3),Nothing),((2,4),Nothing),((2,5),Nothing),((2,6),Nothing),((3,0),Nothing),((3,1),Nothing),((3,2),Nothing),((3,3),Nothing),((3,4),Nothing),((3,5),Nothing),((3,6),Nothing),((4,0),Nothing),((4,1),Nothing),((4,2),Nothing),((4,3),Nothing),((4,4),Nothing),((4,5),Nothing),((4,6),Nothing),((5,0),Nothing),((5,1),Nothing),((5,2),Nothing),((5,3),Nothing),((5,4),Nothing),((5,5),Nothing),((5,6),Nothing),((6,0),Just Blue),((6,1),Nothing),((6,2),Nothing),((6,3),Nothing),((6,4),Nothing),((6,5),Nothing),((6,6),Nothing)], numbers = array ((0,0),(6,6)) [((0,0),Just 2),((0,1),Just 2),((0,2),Just 2),((0,3),Just 3),((0,4),Just 3),((0,5),Just 1),((0,6),Nothing),((1,0),Nothing),((1,1),Nothing),((1,2),Just 0),((1,3),Just 2),((1,4),Nothing),((1,5),Just 3),((1,6),Nothing),((2,0),Nothing),((2,1),Nothing),((2,2),Nothing),((2,3),Nothing),((2,4),Nothing),((2,5),Nothing),((2,6),Nothing),((3,0),Just 3),((3,1),Just 3),((3,2),Nothing),((3,3),Nothing),((3,4),Just 3),((3,5),Just 3),((3,6),Nothing),((4,0),Nothing),((4,1),Nothing),((4,2),Just 2),((4,3),Nothing),((4,4),Just 2),((4,5),Nothing),((4,6),Nothing),((5,0),Just 3),((5,1),Nothing),((5,2),Just 2),((5,3),Nothing),((5,4),Just 2),((5,5),Just 2),((5,6),Nothing),((6,0),Just 3),((6,1),Nothing),((6,2),Just 2),((6,3),Nothing),((6,4),Nothing),((6,5),Just 3),((6,6),Nothing)]}

main :: IO ()
main = showSlitherlink s

solve :: Slitherlink -> IO ()
solve s = do
    showSlitherlink $! s
    threadDelay (2000000)
    recurOn (Slither.update s $ applyAllRulesSoFar s) s

recurOn :: Slitherlink -> Slitherlink -> IO ()
recurOn s olds = if s == olds 
    then do
        showSlitherlink $! s
    else do
        showSlitherlink $! s
        threadDelay (2000000)
        recurOn (Slither.update s $ applyAllRulesSoFar s) s


