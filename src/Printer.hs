module Printer (showSlitherlink) where

import qualified Data.Map.Strict as M
import System.Console.ANSI
import qualified System.Console.ANSI as ANSI
import Data.Maybe

import Slither

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

getLineOrientation :: Slither.Line -> LineOrientation
getLineOrientation (Line((r1, _), (r2, _))) =
    if r1 == r2 then H else V

bluebox = [SetColor Background Vivid ANSI.Cyan]
yellowbox = [SetColor Background Vivid ANSI.Yellow]
bg = [SetColor Background Vivid ANSI.White]
x = "\x00D7"
vline = "|" -- "\x2503"
hline = "-" -- "\x2501"

showPoint :: Slitherlink -> Slither.Point -> IO ()
showPoint s (p@(Point(r, c))) = do
    setCursorPosition (2*r) (2*c)
    setSGR bg
    putStr "\x00B7" -- for now

showBox :: Slitherlink -> Slither.Box -> IO ()
showBox s b@(Box(r, c)) = do
    setCursorPosition (2*r+1) (2*c+1)
    setSGR (case (boxColor s b) of
        Just Slither.Blue -> bluebox
        Just Slither.Yellow -> yellowbox
        Nothing -> bg)
    putStr (maybe " " show $ boxNum s b)
showLine :: Slitherlink -> Slither.Line -> IO ()
showLine s (l@(Line((r, c), _))) =
    let
        hv = getLineOrientation l
        (r', c') = case hv of
            H -> (2*r, 2*c + 1)
            V -> (2*r + 1, 2*c)
    in do
    setCursorPosition r' c'
    setSGR bg
    putStr (case (hv, getLineType s l) of
        (_, Nothing) -> " "
        (_, Just X) -> x
        (H, _) -> hline
        (V, _) -> vline)

showSlitherlink :: Slitherlink -> IO()
showSlitherlink s = do
    clearScreen
    setCursorPosition 0 0
    setSGR bg
    setSGR [SetColor Foreground Vivid Black]
    mapM_ (showPoint s) $ getPoints s
    mapM_ (showBox s) $ getBoxes s
    mapM_ (showLine s) $ getLines s
    cursorDownLine 2
    setCursorColumn 0
    setSGR [Reset]









