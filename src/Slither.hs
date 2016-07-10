{-# LANGUAGE DisambiguateRecordFields #-}
module Slither (BoxColor(..), LineType(..), Slitherlink(..),
    Box(..), Line(..), Point(..), Updates, update,
    getLines, getBoxes, getPoints, pointAdjPoints, pointIncidentLines,
    getLineType, makeSlither, boxNum, boxColor, lineAdjBoxes,
    boxAdjBoxes, boxIncidentLines) where

import qualified Data.Map.Strict as M
import qualified Data.Array as A
import qualified Data.Set as S
import Data.Maybe (catMaybes)
-- import qualified Data.UnionFind.IntMap as UF
import qualified Color
import Control.Monad

-- data GameState board = Unsolvable | Solved board | InProgress board
--     deriving (Show)

-- instance Monad GameState where
--     board >>= transform = case board of
--         InProgress b -> transform b
--         other -> other
--     return b -> InProgress b

    

type BoxColor = IntMap

data LineType = L | X deriving (Eq, Enum, Show)

type NumAdj = Maybe Int

type Coord = (Int, Int) -- Row, col from the top left.

data SlitherBoard = Board { size :: (Int, Int) -- row, col of block rows
                          , numbers :: A.Array Coord NumAdj
                          } deriving (Show, Eq)
a
data GameState = GameState { board :: SlitherBoard
                           , lines :: M.Map Line (Maybe LineType)
                           -- , boxes :: A.Array Coord (Maybe BoxColor)
                           , colors :: A.Array Coord (UF.Point ())
                           , colormap :: UF.PointSupply
                           , blue :: UF.Point ()
                           , yellow :: UF.Point ()
                           , recentlyUpdated :: [Element]
                           } deriving (Show)

-- data Update = BoxUpdate(Box, BoxColor)
--             | LineUpdate(Line, LineType)

--  * - * - * - *
--  | # | # | # |
--  * - * - * - *
--  | # | # | # |
--  * - * - * - *
--  | # | # | # |
--  * - * - * - *


newtype Box = Box Coord deriving (Eq, Show)
newtype Line = Line (Coord, Coord) deriving (Eq, Show, Ord)
newtype Point = Point Coord deriving (Eq, Show)
data Element = ABox (Box) | ALine (Line) deriving (Show)

getLines :: Slitherlink -> [Line]
getLines = M.keys . Slither.lines

getBoxes :: Slitherlink -> [Box]
getBoxes = map Box . A.indices . boxes

getPoints :: Slitherlink -> [Point]
getPoints s = map Point $ A.range ((0, 0), size s)

canonicalLine :: Point -> Point -> Line
canonicalLine (Point p1) (Point p2) =
    | p1 <= p2  = Line (p1, p2)
    | otherwise = Line (p2, p1)


coordNeighbors :: Coord -> Coord -> Coord -> [Coord]
coordNeighbors lo hi (row, col) = catMaybes $ map (checkBounds lo hi) neighbors
    where neighbors = [(row-1, col), (row+1, col), (row, col-1), (row, col+1)]

pointAdjPoints :: Slitherlink -> Point -> [Point]
pointAdjPoints s (Point coord) =
    map Point neighbors
    where 
        neighbors = coordNeighbors (0, 0) (maxrow + 1, maxcol + 1) coord
        (maxrow, maxcol) = size s

pointAdjBoxes :: Slitherlink -> Point -> [Box]
pointAdjBoxes s (Point (r, c)) = map Box $ catMaybes 
    $ map (checkBounds (0, 0) (size s)) 
    [(r, c), (r, c-1), (r-1, c), (r-1, c-1)]    

pointIncidentLines :: Slitherlink -> Point -> [Line]
pointIncidentLines s p = map (canonicalLine p) $ pointAdjPoints s p

boxIncidentLines :: Slitherlink -> Box -> [Line]
boxIncidentLines s (Box (r, c)) =
    let
        p1 = Point (r,c)
        p2 = Point (r,c+1)
        p3 = Point (r+1,c)
        p4 = Point (r+1,c+1)
        -- 1   2
        --  BOX
        -- 3   4
    in
        [ (canonicalLine p1 p2)
        , (canonicalLine p3 p4)
        , (canonicalLine p1 p3)
        , (canonicalLine p2 p4)
        ]

boxAdjBoxes :: Slitherlink -> Box -> [Box]
boxAdjBoxes s (Box coord) = map Box neighbors
    where
        neighbors = coordNeighbors (0, 0) (size s) coord


--(above/left, down/right)
lineAdjBoxes :: Slitherlink -> Line -> (Maybe Box, Maybe Box)
lineAdjBoxes s (Line((r1, c1), (r2, c2))) =
    let
        prev = if r1 == r2 then (r1 - 1, c1) else (r1, c1-1)
        next = (r1, c1)
        check = checkBounds (0, 0) $ size s
    in (fmap Box $ check prev, fmap Box $ check next)

boxColor :: Slitherlink -> Box -> Maybe BoxColor
boxColor s (Box coord) = (boxes s) A.! coord

boxNum :: Slitherlink -> Box -> NumAdj
boxNum s (Box coord) = (numbers s) A.! coord

-- Replaces Off-the-grid values with yellow
-- boxGetAdjColors :: Slitherlink -> Box -> Adj Color
-- boxGetAdjColors s = fmap (maybe Yellow $ boxColor s) . boxGetAdjBoxes s

makeSlitherBoard :: Coord -> [(Coord, Int)] -> SlitherBoard
makeSlitherBoard (rows, cols) nums =
    SlitherBoard { size = (rows, cols)
                 , numbers = A.array ((0, 0), (rows-1, cols-1)) nums
                 }



newGame :: SlitherBoard -> GameState
newGame sb = 
    let
        accumColors coord (state, points) =
            let newstate, nextpoint = fresh state coord
            in (newstate, points:nextpoint)

        indices = A.indices $ 
        colors, colorslist = foldr accumColors UF.newPointSupply $ A.indices 
        maxBoxes = (rows-1, cols-1)
        boxColors = A.listArray ((0, 0), maxBoxes) (repeat Nothing)
        -- emptyBoxNumbers = A.listArray ((0, 0), maxBoxes) (repeat Nothing)
        boxNumList = map (fmap Just) nums
        boxNumbers = A.accum (const id) emptyBoxNumbers boxNumList
        indices = A.range ((0, 0), (rows, cols))

        lineList = ([Line (p, (r+1, c)) | p@(r, c) <- indices, r < rows]
                 ++ [Line (p, (r, c+1)) | p@(r, c) <- indices, c < cols])
        lineTypes = M.fromList (zip lineList (repeat Nothing))
    in
        GameState { board = sb
                  , lines = lineTypes
                  , colors = undefined
                  , colormap = undefined
                  , blue = undefined
                  , yellow = undefined
                  ,recentlyUpdated = undefined
                  }


getLineType :: Slitherlink -> Line -> Maybe LineType
getLineType s l = (Slither.lines s) M.! l

-- updateLines :: Slitherlink -> [(Line, LineType)] -> Slitherlink
-- updateLines s l = Slither { size=size s
--                           , lines = updated
--                           , boxes = boxes s
--                           , numbers = numbers s
--                           }
--     where
--         updates = M.fromList l
--         updated = M.union updates $ Slither.lines s

-- TODO: check no conflicts?
-- updateBoxes :: Slitherlink -> [(Box, Color)] -> Slitherlink
-- updateBoxes s l = Slither { size = size s
--                           , lines = Slither.lines s
--                           , boxes = newboxes s
--                           , numbers = numbers s
--                           }
--     where
--         newboxes = A.accum (const id) l $ boxes s

update :: Slitherlink -> Updates -> Slitherlink
update s (lineupdates, boxupdates) = let
        updates = M.fromList $ map (fmap Just) lineupdates
        newlines = M.union updates $ Slither.lines s
        newboxes = boxes s A.// map (\(Box b, new) -> (b, Just new)) boxupdates
    in
         Slither { size = size s
                 , lines = newlines
                 , boxes = newboxes
                 , numbers = numbers s
                 }


