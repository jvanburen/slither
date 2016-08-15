{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Lib
import Color
import Slither
import Logic
import Data.Maybe
import Control.Monad
import Printer
import qualified Data.Map as M
import Adj
import Control.Concurrent (threadDelay)

testboard1 = Slither.makeSlitherBoard (7, 7) 
    [ ((0, 0), 2)
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
    -- , ((3, 6), 2)
    -- , ((4, 6), 2)
    -- , ((5, 6), 1)
    -- , ((6, 6), 3)
    ]

testboard2 = Slither.makeSlitherBoard (5, 5) 
    [((0, 0), 3), ((1, 1), 2), ((2, 2), 3)]

testgs = Slither.newGame testboard1
startRules = map start3Rules (getBoxes testgs)
testgs2 = fromJust $ foldM (flip ($)) testgs startRules


theboxes = getBoxes testgs2
checkadjlines gs box = do
    putStr "Box: "
    print box
    let Adj{up, down, left, right} = Slither.boxIncidentLines box
    putStr $ show up ++ ": "
    print $ (Slither.lines gs) M.! up
    putStr $ show down ++ ": "
    print $ (Slither.lines gs) M.! down
    putStr $ show left ++ ": "
    print $ (Slither.lines gs) M.! left
    putStr $ show right ++ ": "
    print $ (Slither.lines gs) M.! right


show_progress rules gs = 
    if viewUpdates gs == "Dequeue []" then Printer.showGameState gs
    else do
        Printer.showGameState gs
        putStr "Next Update: "
        putStrLn $ peekNextUpdate gs
        -- threadDelay (250000)
        gs <- pure $ reduceStateOnce rules gs
        case gs of 
            Nothing -> putStrLn "Unsolvable!"
            Just gs -> show_progress rules gs


main :: IO ()
main = do 
    -- forM theboxes (checkadjlines testgs2)
    case reduceState theRules testgs2 of
        Nothing -> print "oops, nothing!"
        Just gs -> Printer.showGameState gs




