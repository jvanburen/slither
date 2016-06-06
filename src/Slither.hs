{-# LANGUAGE DisambiguateRecordFields #-}
module Slither (Color(..), LineType(..), NumAdj(..), Coord, Adj, Slitherlink, 
    Box(..), Line(..), Point(..), Updates, update,
    getLines, getBoxes, getPoints, pointGetAdj, pointGetLines, 
    getLineType, makeSlither, boxGetNum, boxColor, lineAdjBoxes, 
    boxGetAdjBoxes, boxGetIncidentLines, boxGetAdjColors) where

import qualified Data.Map.Strict as M
import qualified Data.Array as A
import qualified Data.Set as S

data Color = Empty | Blue | Yellow deriving (Eq, Enum, Show)

data LineType = Tbd | Solid | X deriving (Eq, Enum, Show)

data NumAdj = Unknown | Zero | One | Two | Three  deriving (Eq, Enum, Show)

type Coord = (Int, Int) -- Row, col from the top left.

data Slitherlink = Slither { size :: (Int, Int) -- row, col of block rows, not points
                           , lines :: M.Map Line LineType
                           , boxes :: A.Array Coord Color
                           , numbers :: A.Array Coord NumAdj
                           } deriving (Show)

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

type Updates = ([(Line, LineType)], [(Box, Color)])

getLines :: Slitherlink -> [Line]
getLines = M.keys . Slither.lines

getBoxes :: Slitherlink -> [Box]
getBoxes = map Box . A.indices . boxes

getPoints :: Slitherlink -> [Point]
getPoints s = map Point $ A.range ((0, 0), size s)

canonicalLine :: Point -> Point -> Line
canonicalLine (Point p1) (Point p2)
    | p1 <= p2  = Line (p1, p2)
    | otherwise = Line (p2, p1)

data Adj a = Adj { above :: a
                 , below :: a
                 , left :: a
                 , right :: a
                 } deriving (Eq, Show)

instance Functor Adj where
    fmap f (Adj up dn l r) = Adj (f up) (f dn) (f l) (f r)

checkBounds :: Coord -> Coord -> Coord -> Maybe Coord
checkBounds (loRow, loCol) (hiRow, hiCol) (row, col)
    | row < loRow  = Nothing
    | row >= hiRow = Nothing
    | col < loCol  = Nothing
    | col >= hiCol = Nothing
    | otherwise    = Just (row, col)

coordNeighbors :: Coord -> Coord -> Coord -> Adj (Maybe Coord)
coordNeighbors lo hi (row, col) = fmap (checkBounds lo hi) neighbors
    where neighbors = Adj (row-1, col) (row+1, col) (row, col-1) (row, col+1)

pointGetAdj :: Slitherlink -> Point -> Adj (Maybe Point)
pointGetAdj s (Point coord) =
    fmap (fmap Point) neighbors
    where neighbors = coordNeighbors (0, 0) (size s) coord

pointGetLines :: Slitherlink -> Point -> Adj (Maybe Line)
pointGetLines s p = fmap (fmap $ canonicalLine p) $ pointGetAdj s p

boxGetIncidentLines :: Slitherlink -> Box -> Adj Line
boxGetIncidentLines s (Box (r, c)) =
    let
        p1 = (r,c)
        p2 = (r,c+1)
        p3 = (r+1,c)
        p4 = (r+1,c+1)
        -- 1   2
        --  BOX
        -- 3   4

        -- Adj _ (Just d1) _ (Just d2) = pointGetAdj (Point coord)
        -- Adj (Just left) _ _ (Just bot) = pointGetLines d1
        -- Adj _ (Just r) (Just top) _ = pointGetLines d2
    in
        fmap Line $ Adj (p1, p2) (p3, p4) (p2, p3) (p2, p4)

boxGetAdjBoxes :: Slitherlink -> Box -> Adj (Maybe Box)
boxGetAdjBoxes s (Box coord) =
    fmap (fmap Box) neighbors
    where 
        (rows, cols) = size s
        neighbors = coordNeighbors (0, 0) (rows - 1, cols - 1) coord

boxGetNum :: Slitherlink -> Box -> NumAdj
boxGetNum s (Box b) = (numbers s) A.! b

--(above/left, down/right)
lineAdjBoxes :: Slitherlink -> Line -> (Maybe Box, Maybe Box)
lineAdjBoxes s (Line((r1, c1), (r2, c2))) =
    let
        prev = (2*r1 - r2, 2*c1 - c2)
        next = (r1, c1)
        check = checkBounds (0, 0) $ size s
    in (fmap Box $ check prev, fmap Box $ check next)

boxColor :: Slitherlink -> Box -> Color
boxColor s (Box coord) = (boxes s) A.! coord

boxNum :: Slitherlink -> Box -> NumAdj
boxNum s (Box coord) = (numbers s) A.! coord

-- Replaces Off-the-grid values with yellow
boxGetAdjColors :: Slitherlink -> Box -> Adj Color
boxGetAdjColors s = fmap (maybe Yellow $ boxColor s) . boxGetAdjBoxes s

makeSlither :: Coord -> [(Coord, Int)] -> Slitherlink
makeSlither (rows, cols) nums =
    let
        maxBoxes = (rows-1, cols-1)
        boxColors = A.listArray ((0, 0), maxBoxes) (repeat Empty)
        emptyBoxNumbers = A.listArray ((0, 0), maxBoxes) (repeat Unknown)
        boxNumList = map (\(a,b) -> (a, intToBoxNum b)) nums
        boxNumbers = A.accum (const id) emptyBoxNumbers boxNumList
        indices = A.range ((0, 0), (rows, cols))
        lineList = ([Line (p, (r+1, c)) | p@(r, c) <- indices, r < rows]
                 ++ [Line (p, (r, c+1)) | p@(r, c) <- indices, c < cols])
        lineTypes = M.fromList (zip lineList (repeat Tbd))
    in
        Slither { size = (rows, cols)
                , lines = lineTypes
                , boxes = boxColors
                , numbers = boxNumbers
                }

intToBoxNum :: Int -> NumAdj
intToBoxNum 0 = Zero
intToBoxNum 1 = One
intToBoxNum 2 = Two
intToBoxNum 3 = Three
intToBoxNum _ = error "why"

getLineType :: Slitherlink -> Line -> LineType
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
        updates = M.fromList lineupdates
        newlines = M.union updates $ Slither.lines s
        newboxes = boxes s A.// map (\(Box b, new) -> (b, new)) boxupdates
    in
         Slither { size = size s
                 , lines = newlines
                 , boxes = newboxes
                 , numbers = numbers s
                 }


