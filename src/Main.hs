module Main where

import Slither
import Printer
import Solver

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
                       ]

main :: IO ()
main = showSlitherlink s
