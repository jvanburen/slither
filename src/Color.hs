{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Color (ColorMap, newColorMap, areOpposite, lookupOpposite, equiv,
              markSame, markOpposite, getColor, isBlue, isYellow, Color, 
              getYellow) where

import qualified Data.UnionFind.IntMap as UF
import qualified Data.Map.Strict as M
import qualified Data.Array as A
import Data.Maybe

type Coord = (Int, Int)
type Color = UF.Point Coord

-- map from repr points to non-repr points (only stores points with opposites)
type CoordColorMap = M.Map Coord Color

data ColorMap = ColorMap { ps :: UF.PointSupply Coord
                         , points :: A.Array Coord Color
                         -- , blue :: Color -- sentinal coordinate value
                         , yellow :: Color  -- sentinal coordinate value
                         , opposites :: CoordColorMap
                         }
instance Show ColorMap where
    show c = "ColorMap"

instance Show Color where
    show c = "SomeColor"
-- sentinalBlue = (-1, 0) :: Coord
sentinalYellow = (0, -1) :: Coord

-- Constructor
newColorMap :: (Coord, Coord) -> ColorMap
newColorMap (start, end) = 
    let
        accumColors coord (state, points) = (newstate, point:points)
            where (newstate, point) = UF.fresh state coord
        indices = A.range (start, end)
        (ps, pointsList) = foldr accumColors (UF.newPointSupply, []) indices
        -- (ps2, blue) = UF.fresh ps sentinalBlue
        -- (ps3, yellow) = UF.fresh ps2 sentinalYellow
        (ps3, yellow) = UF.fresh ps sentinalYellow
        -- opps = M.fromList [(sentinalBlue, yellow), (sentinalYellow, blue)]
        opps = M.empty
        pointsArray = A.listArray (start, end) pointsList
    in
        ColorMap ps3 -- ps
                 pointsArray -- points
                 -- blue -- blue
                 yellow -- yellow
                 opps -- opposites
                 

-- Wrapper for the lazy
equiv :: ColorMap -> Color -> Color -> Bool
equiv (ColorMap{ps}) = UF.equivalent ps

-- Wrapper for the lazy
union :: ColorMap -> Color -> Color -> UF.PointSupply Coord
union (ColorMap{ps}) = UF.union ps

-- Wrapper for the lazy
repr :: ColorMap -> Color -> Color
repr (ColorMap{ps}) = UF.repr ps

coord :: ColorMap -> Color -> Coord
coord (ColorMap{ps}) = UF.descriptor ps

-- Get a list of coordinates with the same color as given
sameColors :: ColorMap -> Color -> [Coord]
sameColors cmap c = map (coord cmap) $ filter (equiv cmap c) (A.elems $ points cmap)

-- Get the opposite of a color (if it exists)
lookupOpposite :: ColorMap -> Color -> Maybe Color
lookupOpposite cmap c = M.lookup (coord cmap $ repr cmap c) $ opposites cmap

-- Test if two colors are necessarily opposites
areOpposite :: ColorMap -> Color -> Color -> Bool
areOpposite cmap c =  maybe False (equiv cmap c) . lookupOpposite cmap

mapDeleteLookup = M.updateLookupWithKey removeItem
    where removeItem x y = Nothing

getColor :: ColorMap -> Coord -> Color
getColor (ColorMap{points, yellow}) coord =
    if A.inRange (A.bounds points) coord
        then points A.! coord
    else
        yellow

getYellow = yellow

isYellow :: ColorMap -> Color -> Bool
isYellow cmap = equiv cmap (yellow cmap)

isBlue :: ColorMap -> Color -> Bool
isBlue cmap = areOpposite cmap (yellow cmap)

-- Internal use only
-- Combines two colors and gets rid of outgoing opposites
-- also returns those opposites
-- returns the new cmap, the opposite of the first, the opposite of the second
internalUnionGetOpps :: ColorMap -> Color -> Color -> (ColorMap, Maybe Color, Maybe Color)
internalUnionGetOpps (cmap@ColorMap{ps, opposites=opps}) c1 c2 = 
    let
        r1 = UF.repr ps c1
        r2 = UF.repr ps c2

        (opp1, opps1) = mapDeleteLookup (UF.descriptor ps r1) opps
        (opp2, opps2) = mapDeleteLookup (UF.descriptor ps r2) opps1
        combined = UF.union ps r1 r2
    in
        (cmap {ps=combined, opposites=opps2}, opp1, opp2)

internalMarkOpposite :: ColorMap -> Color -> Color -> ColorMap
internalMarkOpposite (cmap@ColorMap{ps, opposites=opps}) c1 c2 = 
    let
        r1 = UF.repr ps c1
        r2 = UF.repr ps c2
        opps1 = M.insert (UF.descriptor ps r2) r1 opps
        opps2 = M.insert (UF.descriptor ps r1) r2 opps1
    in
        cmap{opposites=opps2}


internalMarkSame :: ColorMap -> Color -> Color -> ColorMap
internalMarkSame cmap c1 c2 = let
        (cmap2, opp1, opp2) = internalUnionGetOpps cmap c1 c2
    in case (opp1, opp2) of
        (Nothing, Nothing) -> cmap2
        (Just opp1, Nothing) -> internalMarkOpposite cmap2 c1 opp1
        (Nothing, Just opp2) -> internalMarkOpposite cmap2 c1 opp2
        (Just opp1, Just opp2) -> let 
            (cmap3, _, _) = internalUnionGetOpps cmap2 opp1 opp2
            in internalMarkOpposite cmap3 c1 opp1


markSame :: ColorMap -> Color -> Color -> Maybe (ColorMap, [Coord])
markSame cmap c1 c2 =
    if equiv cmap c1 c2
        then Just (cmap, [])  -- No changes made, ez
    else if areOpposite cmap c1 c2
        then Nothing          -- Contradiction
    else let
        newcmap = internalMarkSame cmap c1 c2
        updated = (sameColors newcmap c1) 
            ++ maybe [] (sameColors newcmap) (lookupOpposite newcmap c1) 
    in 
        Just (newcmap, updated)


markOpposite :: ColorMap -> Color -> Color -> Maybe (ColorMap, [Coord])
markOpposite cmap c1 c2 = 
     if equiv cmap c1 c2
        then Just (cmap, [])  -- No changes made, ez
    else if areOpposite cmap c1 c2
        then Nothing          -- Contradiction
    else case (lookupOpposite cmap c1, lookupOpposite cmap c2) of
        (Nothing, Nothing) -> let 
            newcmap = internalMarkOpposite cmap c1 c2
            updated = (sameColors newcmap c1) 
                ++ maybe [] (sameColors newcmap) (lookupOpposite newcmap c1)
            in Just (newcmap, updated) 
        (Just opp1, Nothing) -> markSame cmap opp1 c2
        (_, Just opp2) -> markSame cmap c1 opp2

