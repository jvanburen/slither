module Printer (showGameState) where

import qualified Data.Map.Strict as M
import System.Console.ANSI
import qualified System.Console.ANSI as ANSI
import Data.Maybe
import Adj
import Slither
-- import Logic (maybeBoxColor)

--  * - * - * - *
--  | # | # | # |
--  * - * - * - *
--  | # | # | # |
--  * - * - * - *
--  | # | # | # |
--  * - * - * - *

-- screw trying to make it good, just jump to the right bits and emit a lot of control codes (for now).

-- drawBox

-- data Printable =
--     Point | PointU | PointR | PointD | PointR
--     | PointUR | PointUL | PointUD

data LineOrientation = H | V deriving (Show, Eq)

getLineOrientation :: GameState -> Slither.Line -> LineOrientation
getLineOrientation gs l =
    let
        (p1, p2) = lineIncidentPoints l
        adj = pointAdjPoints gs p1
    in
        if right adj == Just p2 then H else V

bluebox = [SetColor Background Vivid ANSI.Cyan]
yellowbox = [SetColor Background Vivid ANSI.Yellow]
bg = [SetColor Background Vivid ANSI.White]
x = "\x2573 " -- "\x00D7"

-- https://en.wikipedia.org/wiki/Block_Elements

vline = "\x2503 " -- "\x2590\x258C" -- "\x2503"
hline = "\x2501\x2501" --     "\x2584\x2584" -- "\x2501"
bigX = "\x2573 " -- space included


-- bignums = "\xFF10\xFF11\xFF12\xFF13\xFF14\xFF15\xFF16\xFF17\xFF18\xFF19"
-- bignums = "0123456789"
pad n = if n < 10 then (show n) ++ " " -- (bignums !! n):" "
    else (show n)

showPoint :: GameState -> Point -> IO ()
showPoint gs p = do
    let (r, c) = getPoint p
    setCursorPosition (2*r) (4*c)
    setSGR bg
    putStr "\xFF65 " -- for now

showBox :: GameState -> Slither.Box -> IO ()
showBox gs b = do
    let (r, c) = getBox b
    setCursorPosition (2*r+1) (4*c+2)
    let bc = boxColor gs b
    setSGR (if isBlue gs bc
                then bluebox
            else if isYellow gs bc
                then yellowbox
            else
                bg
            )
    putStr (maybe "  " pad $ boxNum gs b)
showLine :: GameState -> Slither.Line -> IO ()
showLine gs l =
    let
        (p1, p2) = lineIncidentPoints l
        (r, c) = getPoint p1
        hv = getLineOrientation gs l
        (r', c') = case hv of
            H -> (2*r, 4*c + 2)
            V -> (2*r + 1, 4*c)
        (c1, c2) = lineIncidentColors gs l

    in do
    setCursorPosition r' c'
    if Slither.isBlue gs c1 then
        if Slither.isBlue gs c2
        then setSGR bluebox else setSGR bg
    else if Slither.isYellow gs c1
        then if Slither.isYellow gs c2
            then setSGR yellowbox else setSGR bg
    else setSGR bg

    putStr (case (hv, lineType gs l) of
        (_, Nothing) -> "  "
        (_, Just X) -> x
        (H, _) -> hline
        (V, _) -> vline)

showGameState :: GameState -> IO()
showGameState gs = do
    clearScreen
    setCursorPosition 0 0
    setSGR bg
    setSGR [SetColor Foreground Vivid Black]
    mapM_ (showBox gs) $ getBoxes gs
    mapM_ (showLine gs) $ getLines gs
    mapM_ (showPoint gs) $ getPoints gs
    cursorDownLine 2
    setCursorColumn 0
    setSGR [Reset]
