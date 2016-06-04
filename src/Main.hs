module Main where

import Slither
import Printer

s = makeSlither (3, 3) [((0, 0), 2), ((1, 2), 0), ((2, 1), 2)]

main :: IO ()
main = do
  showSlitherlink s

