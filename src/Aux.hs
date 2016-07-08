
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List

graphSearch :: (Ord a) => (a -> [a]) -> a -> S.Set
graphSearch getAdj init = dfs getAdj (Set.empty ()) init

dfs :: (Ord a) => (a -> [a]) -> S.Set a -> a -> S.Set
dfs getAdj seen start = 
	if start `S.member` seen then seen
	else foldl' (dfs getAdj) (S.insert start seen) (getAdj start)

