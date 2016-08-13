module Aux (graphSearch) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Maybe

graphSearch :: (Ord a) => (a -> [a]) -> a -> S.Set a
graphSearch getAdj init = dfs getAdj S.empty init

dfs :: (Ord a) => (a -> [a]) -> S.Set a -> a -> S.Set a
dfs getAdj seen start = 
	if start `S.member` seen then seen
	else foldl' (dfs getAdj) (S.insert start seen) (getAdj start)

-- data Adj a = Adj { above :: a
--                  , below :: a
--                  , left :: a
--                  , right :: a
--                  } deriving (Eq, Show)

-- instance Functor Adj where
--     fmap f (Adj up dn l r) = Adj (f up) (f dn) (f l) (f r)

-- type Coord = (Int, Int)

-- checkBounds :: Coord -> Coord -> Coord -> Maybe Coord
-- checkBounds (loRow, loCol) (hiRow, hiCol) (row, col)
--     | row < loRow  = Nothing
--     | row >= hiRow = Nothing
--     | col < loCol  = Nothing
--     | col >= hiCol = Nothing
--     | otherwise    = Just (row, col)

-- coordNeighbors :: Coord -> Coord -> Coord -> [Coord]
-- coordNeighbors lo hi (row, col) = catMaybes $ map (checkBounds lo hi) neighbors
--     where neighbors = [(row-1, col), (row+1, col), (row, col-1), (row, col+1)]