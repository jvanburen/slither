module Logic where

import Slither
import qualified Color as Clr
import Data.Maybe (maybe)

-- noUpdates :: Slither.Updates
-- noUpdates = []

-- updateBoxes :: [(Box, BoxColor)] -> [Slither.Update]
-- updateBoxes = map BoxUpdate

-- updateLines :: [(Line, LineType)] -> [Slither.Updates]
-- updateLines = map LineUpdate

-- -- None will map to Yellow, to signify "off the board"
-- maybeBoxColor :: Slitherlink -> Maybe Box -> Maybe BoxColor
-- maybeBoxColor s = maybe (Just Yellow) (boxColor s)

-- unsolvedEdges :: Slitherlink -> [Edge]
-- unsolvedEdges = filter (\l -> getLineType l == None) . getLines

-- isLineType :: Slitherlink -> Maybe LineType -> Line -> Bool
-- isLineType s typ l = (getLineType s l == typ)

-- isBoxColor :: Slitherlink -> Maybe BoxColor -> Box -> Bool
-- isBoxColor s col b = (boxColor s b == col)

-- setLinesTo :: LineType -> [Line] -> Slither.Updates
-- setLinesTo typ = updateLines . map (\line -> (line, typ))

-- setBoxesTo :: BoxColor -> [Box] -> Slither.Updates
-- setBoxesTo col = updateBoxes . map (\box -> (box, col))


-- setOppColors
--     acts on GS and doesn't add updates? or does
-- addUpdates
--     acts on GS also

-- oppColor Blue = Yellow
-- oppColor Yellow = Blue

lineColorRule :: Slither.Rule
lineColorRule (SetLineTo line L) gs =
    Just $ Slither.sepColors c1 c2 gs
    where (c1, c2) = Slither.lineGetAdjColors gs line
lineColorRule (SetLineTo line X) gs =
    Just $ Slither.joinColors c1 c2 gs
    where (c1, c2) = Slither.lineGetAdjColors gs line
lineColorRule _ = Just

-- commented
    -- lineColorRule :: Slitherlink -> Line -> Slither.Updates
    -- lineColorRule s line = let
    --     lineType = getLineType s line
    --     (b1, b2) = lineAdjBoxes s line
    --     in case lineType of
    --         Nothing -> case (maybeBoxColor s b1, maybeBoxColor s b2) of
    --             (Nothing, _) -> noUpdates
    --             (_, Nothing) -> noUpdates
    --             (Just c1, Just c2) -> if c1 == c2
    --                 then updateLines [LineUpdate (line, X)]
    --                 else updateLines [LineUpdate (line, L)]
    --         Just X -> case (maybeBoxColor s b1, maybeBoxColor s b2) of
    --             (Just c1, Nothing) ->
    --                 maybe noUpdates (\b2 -> updateBoxes [(b2, c1)]) b2
    --             (Nothing, Just c2) ->
    --                 maybe noUpdates (\b1 -> updateBoxes [(b1, c2)]) b1
    --             (mc1, mc2) -> if mc1 == mc2 then noUpdates
    --                 else error $ (show mc1) ++ (show mc2) ++ "unsolvable slitherlink"
    --         Just L -> case (maybeBoxColor s b1, maybeBoxColor s b2) of
    --             (Just c1, Nothing) ->
    --                 maybe noUpdates (\b2 -> updateBoxes [(b2, oppColor c1)]) b2
    --             (Nothing, Just c2) ->
    --                 maybe noUpdates (\b1 -> updateBoxes [(b1, oppColor c2)]) b1
    --             (Nothing, Nothing) -> noUpdates
    --             (mc1, mc2) -> if mc1 /= mc2 then noUpdates
    --                 else error $ (show mc1) ++ (show mc2) ++ "unsolvable slitherlink"

-- make more general!
-- change type to (Maybe Int) I guess
-- boxLineRule :: Slitherlink -> Box -> Slither.Updates
-- boxLineRule s box = case (boxNum s box) of
--     Nothing -> noUpdates
--     Just num -> inferLines s box num
--     where
--         inferLines s box num = let
--             incident = boxIncidentLines s box
--             ls = filter (isLineType s $ Just L) incident
--             xs = filter (isLineType s $ Just X) incident
--             tbds = filter (isLineType s Nothing) incident
--             in
--                 if length tbds + length ls < num || length ls > num
--                     then error "boxlinerule: unsolvable slitherlink"
--                 else if length tbds + length ls == num
--                     then setLinesTo L tbds
--                 else if length ls == num
--                     then setLinesTo X tbds
--                 else noUpdates

{-_-}

-- setLinesTo :: (Foldable t) => LineType -> GameState -> t Slither.Line -> GameState
-- setLinesTo lt = foldr (insertUpdate . SetLineTo lt)

-- getOtherBox b (b1, b2) = if b == b1 then b2 else b1

innerBoxLineRule :: Slither.Box -> GameState -> Maybe GameState
innerBoxLineRule box gs = case (boxNum gs box) of
    Nothing -> Just gs
    Just num -> let
            incident = toList $ Slither.boxIncidentLines gs box
            lines = filter (isLineType gs $ Just L) incident
            -- xs = filter (isLineType gs $ Just X) incident
            tbds = filter (isLineType gs Nothing) incident
        in
            if length tbds == 0
            || length lines + length tbds < num 
                then Just gs
            else if length lines + length tbds == num
                then Just $ setLinesTo L gs tbds
            else if length lines == num
                then Just $ setLinesTo X gs tbds
            else case tbds of
                [b1, b2] -> Just $ Slither.sepColors (boxColor gs b1) (boxColor gs b2)
                _ -> Just gs

boxLineRule :: Slither.Rule
-- boxLineRule (SetLineTo _ line) gs = 
--     let 
--         (b1, b2) = Slither.lineGetAdjBoxes gs line
--         f1 = maybe Just innerboxLineRule b1
--         f2 = maybe Just innerboxLineRule b2
--     in
--         f1 >>= f2 $ gs
boxLineRule (SetLineTo _ line) gs =         
    innerboxLineRule b1 gs >>= innerboxLineRule b2
    where (b1, b2) = Slither.lineGetAdjBoxes gs line
boxLineRule _ = Just

-- Old code
    -- adjacent 3s rule
    -- adjThrees :: Slitherlink -> Box -> Slither.Updates
    -- adjThrees s box = case boxNum s box of
    --  Just 3 ->

    -- cornerRules :: Slitherlink -> Box -> Slither.Updates
    -- cornerRules s box =
    --     case (boxNum s box) of
    --         Nothing -> noUpdates
    --         Just num ->
    --             if length (boxAdjBoxes s box) < num
    --                 then updateBoxes [(box, Blue)]
    --             else if num <
    --                 length (boxIncidentLines s box) - length (boxAdjBoxes s box)
    --                 then updateBoxes [(box, Yellow)]
    --             else noUpdates

-- line continuing
-- lineContinueRule :: Slitherlink -> Point -> Slither.Updates
-- lineContinueRule s p = let
--     incident = pointIncidentLines s p
--     ls = filter (isLineType s $ Just L) incident
--     xs = filter (isLineType s $ Just X) incident
--     tbds = filter (isLineType s Nothing) incident
--     in
--         if length ls == 2
--             then setLinesTo X tbds
--         else if length ls == 1 && length tbds == 1
--             then setLinesTo L tbds
--         else if length ls > 2
--             then error "too many lines set"
--         else noUpdates

lineContinue :: Slither.Point -> GameState -> Maybe GameState
lineContinue p gs = 
    let
        incident = pointIncidentLines gs p
        ls = filter (isLineType gs $ Just L) incident
        xs = filter (isLineType gs $ Just X) incident
        tbds = filter (isLineType gs Nothing) incident
    in
        case (length ls, tbds) of
            (2, _) -> Just $ Slither.setLinesTo X tbds gs
            (1, [line]) -> Just $ Slither.setLineTo L line gs
            (0, [line]) -> Just $ Slither.setLineTo
            (1, _) | (0, _) -> Just GC
            _ -> Nothing
            _ -> Just GS

lineContinueRule :: Slither.Rule
lineContinueRule (SetLineTo _ line) gs = 
    lineContinue p1 gs >>= lineContinue p2
    where (p1, p2) = lineIncidentPoints line gs



-- for coloring: check adjacent blocks for same /diff color 
-- Check all numbered blocks? i guess so.


-- 2 coloring rule

-- colorAdj :: Slitherlink -> Box -> Slither.Updates
-- colorAdj s box = let
--     adj = boxAdjBoxes s box
--     numIncident = length $ boxIncidentLines s box
--     blue = filter (isBoxColor s $ Just Blue) adj
--     yellow = filter (isBoxColor s $ Just Yellow) adj
--     idks = filter (isBoxColor s Nothing) adj
--     yellows = length yellow + numIncident - length adj
--     in case (boxNum s box) of
--         Nothing -> noUpdates
--         Just num ->
--             if length blue >= num
--                 && length idks + yellows == num
--                 then setBoxesTo Yellow idks
--             else if yellows >= num
--                 && length idks + length blue == num
--                 then setBoxesTo Blue idks
--             else noUpdates

-- coloringRule2 :: Slither.Rule
-- coloringRule2 

-- isMaxBox s Nothing = False
-- isMaxBox s (Just box) = case boxNum s box of
--     Nothing -> False
--     Just numSolid -> (length $ boxIncidentLines s box) == numSolid + 1

-- adjacent 3s rule
separateMaxBoxes :: Slitherlink -> Line -> Slither.Updates
separateMaxBoxes s line =
    if isMaxBox s b1 && isMaxBox s b2
        then updateLines [(line, L)]
        else noUpdates
    where (b1, b2) = lineAdjBoxes s line

-- 3 cornered by zero rule
-- necessaryLines :: Slitherlink -> Point -> Slither.Updates
-- necessaryLines s p =
--     if any (isMaxBox s) $



-- If this returns True, the board is unsolvable
-- isInvalid :: Slitherlink -> Bool
-- isInvalid = stuff

-- the coloring rules for 1s and 3s also
-- if it has at least 2 neighbors
-- it has to be the same / diff
