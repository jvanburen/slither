module Color where

import qualified Data.UnionFind.IntMap as UF
import qualified Data.Map.Strict as M
import qualified Data.Array as A
import Data.Maybe

type Coord = (Int, Int)
newtype Color = UF.Point Coord

-- map from repr points to non-repr points (only stores points with opposites)
type CoordColorMap = M.Map Coord Colored

data ColorMap = ColorMap { ps :: UF.PointSource
                         , points :: CoordColorMap
                         , blue :: Color -- sentinal coordinate value
                         , yellow :: Color  -- sentinal coordinate value
                         , opposites :: CoordColorMap
                         }

-- Constructor
newColorMap :: [Coord] -> ColorMap
newColorMap = undefined

-- Wrapper for the lazy
equiv :: ColorMap -> Color -> Color -> Bool
equiv cmap = UF.equivalent (ps cmap)


-- Wrapper for the lazy
union :: ColorMap -> Color -> Color -> UF.PointSource
union cmap = UF.union (ps cmap)

-- Wrapper for the lazy
repr :: ColorMap -> Color -> Color
repr cmap = UF.repr (ps cmap)

coord :: ColorMap -> Color -> Coord
coord cmap = UF.descriptor (ps cmap)

-- Get a list of coordinates with the same color as given
sameColors :: ColorMap -> Color -> [Coord]
sameColors cmap c = map (coord cmap) $ filter (equiv cmap c) (points cmap)

-- Get the opposite of a color (if it exists)
lookupOpposite :: ColorMap -> Color -> Maybe Color
lookupOpposite cmap c = M.lookup (coord cmap $ repr cmap c) $ opposites cmap

-- Test if two colors are necessarily opposites
areOpposite :: ColorMap -> Color -> Color -> Bool
areOpposite cmap c = equiv cmap (lookupOpposite cmap c)

-- changes the key of a key value pair (if it exists)
changeKey :: CoordColorMap -> Coord -> Coord -> CoordColorMap
changeKey ccmap kOld kNew =
    case M.updateLookupWithKey (const $ const Nothing) kOld ccmap of
        (Nothing, _) -> ccmap
        (Just val, newmap) -> M.insert kNew val newmap

-- -- Internal use only
-- -- When two colors are unioned, a key in opposites might not be a repr
-- --  anymore, so this restores the invariant that the keys are reprs
-- fixOpposite :: ColorMap -> Color -> ColorMap
-- fixOpposite cmap c =  
--     case M.lookup (coord cmap c) $ opposites cmap of
--         Nothing -> cmap -- no need to update anything
--         Just val -> 
--             let
--                 opps = opposites cmap
--                 newOpps = M.insert (repr cmap c) val $ M.delete c opps
--             in cmap { opposites = newOpps }

-- Requires the colors to be union-able (not opposite)
unionColorsUnsafe :: ColorMap -> Color -> Color -> ColorMap
unionColorsUnsafe cmap c1 c2 =
    if equiv cmap c1 c2 
        then cmap
    else let
        oldr1, oldr2 = coord cmap $ repr cmap c1, coord cmap $ repr cmap c2
        combined = union cmap c1 c2
        newr1 = coord cmap $ UF.repr combined c1
        newr2 = coord cmap $ UF.repr combined c2
        opps1 = changeKey (opposites cmap) oldr1 newr1
        opps2 = changeKey opps1 oldr2 newr2
    in
        cmap { ps = combined, opposites = opps2 }


-- Mark 2 boxes as having opposite colors
-- Fails (returns Nothing) if the colors are the same (a contradiction).
-- If there's no conflict, it returns the new map and the list of coordinates
--  that have had their color affected by the change.
markOpposite :: ColorMap -> Color -> Color -> Maybe (ColorMap, [Coord])
markOpposite cmap c1 c2 =
    if areOpposite cmap c1 c2 
        then Just (cmap, []) 
    else if equiv cmap c1 c2
        then Nothing  -- Contradiction
    else let
        r1, r2 = repr cmap c1, repr cmap c2
        cmap2 = fixOpposite (cmap { ps = union cmap r1 r2 }) r1
        cmap3 = fixOpposite (cmap2) r2
    in
        cmap3

-- need to union c1 and the opposite of c2
-- need to union c2 and the opposite of c1
        


-- Marks 2 boxes as having the same color.
-- Fails (returns Nothing) if the colors are opposites (a contradiction).
-- If there's no conflict, it returns the new map and the list of coordinates
--  that have had their color affected by the change.
-- Also updates the opposites map in case a repr point changed.
union :: ColorMap -> Color -> Color -> Maybe (ColorMap, [Coord])
union cmap c1 c2 =
    if equiv cmap c1 c2
        then Just (cmap, [])  -- No changes made, ez
    else if areOpposite cmap c1 c2
        then Nothing
    else let
        r1, r2 = repr cmap c1, repr cmap c2

        newPS = UF.union (ps cmap)
        sameColors 

    -- need to fix opposites




