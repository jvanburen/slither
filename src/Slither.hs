{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Slither (GameState,
    joinColors, sepColors, setLineTo, setLinesTo,
    areSameColor, areOppColor, isBlue, isYellow,
    getLines, getBoxes, getPoints, Rule,
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Array as A
import qualified Data.Set as S
import Data.Maybe (catMaybes)
import qualified Data.UnionFind.IntMap as UF
import qualified Color as C
import Control.Monad
import Adj
import qualified Data.Dequeue as D

-- data GameState board = Unsolvable | Solved board | InProgress board
--     deriving (Show)

-- instance Monad GameState where
--     board >>= transform = case board of
--         InProgress b -> transform b
--         other -> other
--     return b -> InProgress b


-- We don't want to blow out the stack so instead of recursively updating the board,
--  we have rules return a list of things to update
-- Process:
--  Pop something off the "list" (set) of things to update
--  Actually apply the update the board (changing the things on the board)
--  Run each relevant rule:
--  Rules will take the game state and the most recent change and they will add their updates to the list in the state

-- How do we know which rules are relevant?
-- Good question
-- Who better to decide what rule is relevant than the rule itself!
-- Rules that aren't affected by the update simply act as const id
-- Then we just fold over the list of partially applied rules. Easy.

type Color = C.Color

-- Takes a spot to look at and a game state
-- Returns a game states with updates added via the appropriate methods
-- Or Nothing if the game is unsolvable
type Rule = Update -> GameState -> Maybe GameState

data LineType = L | X deriving (Eq, Enum, Show)

type Coord = (Int, Int) -- Row, col from the top left.

type NumAdj = Maybe Int
type SlitherBoard = A.Array Coord NumAdj

data GameState = GS { board :: SlitherBoard
                    , lines :: M.Map Line (Maybe LineType)
                    , colors :: C.ColorMap
                    , paths :: UF.PointSupply Line
                    , pendingUpdates :: D.BankersDequeue Update
                    } deriving (Show)
type GS = GameState

data Update = JoinColors Color Color
            | SepColors Color Color
            | SetLineTo LineType Line
            deriving (Show)


insertUpdate :: Update -> GS -> GS
insertUpdate update (gs@GS{pendingUpdates}) =
    gs{pendingUpdates=D.pushBack pendingUpdates update}


-- public interface for the updates
-- TODO: possible optimization: checking for contradictions here also
joinColors :: Color -> Color -> GS -> GS
joinColors c1 c2 = insertUpdate (JoinColors c1 c2)


sepColors :: Color -> Color -> GS ->  GS
sepColors c1 c2 = insertUpdate (SepColors c1 c2)
    -- gs{pendingUpdates=S.insert (SepColors c1 c2) pendingUpdates}


setLineTo :: LineType -> Line -> GS -> GS
setLineTo ltype l = insertUpdate (SetLineTo ltype l)


setLinesTo :: (Foldable t) => LineType -> t Line -> GS -> GS
setLinesTo lt = flip $ foldr (insertUpdate . SetLineTo lt)


-- data Update = BoxUpdate(Box, BoxColor)
--             | LineUpdate(Line, LineType)

--  * - * - * - *
--  | # | # | # |
--  * - * - * - *
--  | # | # | # |
--  * - * - * - *
--  | # | # | # |
--  * - * - * - *


newtype Box = Box { getBox :: Coord }
    deriving (Eq, Show, Ord)
newtype Line = Line { getLine :: (Coord, Coord) }
    deriving (Eq, Show, Ord)
newtype Point = Point { getPoint :: Coord }
    deriving (Eq, Show)
data Element = ABox (Box) | ALine (Line) deriving (Show)




-- Color interface to the Game State
-- equiv :: ColorMap -> Color -> Color -> Bool
-- lookupOpposite :: ColorMap -> Color -> Maybe Color
-- areOpposite :: ColorMap -> Color -> Color -> Bool
-- getColor :: ColorMap -> Coord -> Color
-- markSame :: ColorMap -> Color -> Color -> Maybe (ColorMap, [Coord])
-- markOpposite :: ColorMap -> Color -> Color -> Maybe (ColorMap, [Coord])

areSameColor :: GS -> Color -> Color -> Bool
areSameColor (GS{colors}) = C.equiv colors

areOppColor :: GS -> Color -> Color -> Bool
areOppColor (GS{colors}) = C.areOpposite colors

isBlue :: GS -> Color -> Bool
isBlue (GS{colors}) = C.isBlue colors


isYellow :: GS -> Color -> Bool
isYellow (GS{colors}) = C.isYellow colors


getLines :: GS -> [Line]
getLines = M.keys . Slither.lines

getBoxes :: GS -> [Box]
getBoxes = map Box . A.indices . board

getPoints :: GS -> [Point]
getPoints gs = map Point $ A.range $ pointBounds gs

boxBounds :: GS -> (Coord, Coord)
boxBounds = A.bounds . board

pointBounds :: GS -> (Coord, Coord)
pointBounds gs = (c1, (r2+1, c2+1))
    where 
        (c1, (r2, c2)) = boxBounds gs

canonicalLine :: Point -> Point -> Line
canonicalLine (Point p1) (Point p2)
    | p1 <= p2  = Line (p1, p2)
    | otherwise = Line (p2, p1)


coordAdj :: Coord -> Adj Coord
coordAdj (r, c) = Adj (r-1, c) (r+1, c) (r, c-1) (r, c+1)


validateCoord :: (Coord, Coord) -> Coord -> Maybe Coord
validateCoord ((rmin, cmin), (rmax, cmax)) =
    mfilter (\(r, c)-> rmin <= r && r <= rmax && cmin <= c && c <= cmax) . Just


lineIncidentPoints :: Line -> (Point, Point)
lineIncidentPoints (Line (p1, p2)) = (Point p1, Point p2)


pointAdjPoints :: GS -> Point -> Adj (Maybe Point)
pointAdjPoints gs = fmap validate . coordAdj . getPoint
    where
        validate = fmap Point . validateCoord (pointBounds gs)


-- pointAdjBoxes :: GS -> Point -> [Box]
-- pointAdjBoxes s (Point (r, c)) = map Box $ catMaybes
--     $ map (checkBounds (0, 0) (size s))
--     [(r, c), (r, c-1), (r-1, c), (r-1, c-1)]


pointIncidentLines :: GS -> Point -> Adj (Maybe Line)
pointIncidentLines gs (Point c) = 
    fmap (fmap toLine . validate) $ coordAdj c
    where
        validate = validateCoord $ pointBounds gs
        toLine c2
            | c <= c2   = Line (c, c2)
            | otherwise = Line (c2, c)


boxIncidentLines :: GS -> Box -> Adj Line
boxIncidentLines s (Box (r, c)) =
    let
        p1 = (r,c)
        p2 = (r,c+1)
        p3 = (r+1,c)
        p4 = (r+1,c+1)
        -- p1   p2
        --   BOX
        -- p3   p4
    in
        Adj{ up    = Line (p1, p2)
           , down  = Line (p3, p4)
           , left  = Line (p1, p3)
           , right = Line (p2, p4)
           }


maybeValidBox :: GS -> Coord -> Maybe Box
maybeValidBox (GS{board}) c =
    if A.inRange (A.bounds board) c
        then Just $ Box c
        else Nothing


--(above/left, down/right)
lineIncidentBoxes :: GS -> Line -> (Maybe Box, Maybe Box)
lineIncidentBoxes gs (Line((r1, c1), (r2, c2))) =
    let
        prev = if r1 == r2 then (r1 - 1, c1) else (r1, c1-1)
        next = (r1, c1)
        check = validateCoord $ boxBounds gs
    in (fmap Box $ check prev, fmap Box $ check next)


boxAdjBoxes :: GS -> Box -> Adj (Maybe Box)
boxAdjBoxes gs = fmap validate . coordAdj . getBox
    where
        validate = fmap Box . validateCoord (boxBounds gs)


boxColor :: GS -> Box -> C.Color
boxColor (GS{colors}) = C.getColor colors . getBox


boxNum :: GS -> Box -> NumAdj
boxNum (GS{board}) = (board A.!) . getBox


-- Replaces Off-the-grid values with yellow
boxAdjColors :: GS -> Box -> Adj Color
boxAdjColors (gs@GS{colors}) =
    fmap (maybe (C.getYellow colors) $ boxColor gs) . boxAdjBoxes gs


makeSlitherBoard :: Coord -> [(Coord, Int)] -> SlitherBoard
makeSlitherBoard (rows, cols) = 
    A.accumArray (\_ y -> Just y) Nothing ((0, 0), (rows-1, cols-1))


newGame :: SlitherBoard -> GameState
newGame sb =
    let
        indices = A.indices sb
        (_, (rows, cols)) = A.bounds sb
        lineList = ([Line (p, (r+1, c)) | p@(r, c) <- indices, r < rows]
                 ++ [Line (p, (r, c+1)) | p@(r, c) <- indices, c < cols])
        lineTypes = M.fromList (zip lineList (repeat Nothing))
    in
        GS { board = sb
           , lines = lineTypes
           , colors = C.newColorMap $ A.bounds sb
           , paths = undefined
           , pendingUpdates = D.empty
           }


getLineType :: GS -> Line -> Maybe LineType
getLineType s l = (Slither.lines s) M.! l



