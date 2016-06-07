module Logic where

import Slither
import Data.Maybe (maybe)

noUpdates :: Slither.Updates
noUpdates = ([], [])

updateBoxes :: [(Box, BoxColor)] -> Slither.Updates
updateBoxes bs = ([], bs)

updateLines :: [(Line, LineType)] -> Slither.Updates
updateLines ls = (ls, [])

-- None will map to Yellow, to signify "off the board"
maybeBoxColor :: Slitherlink -> Maybe Box -> Maybe BoxColor
maybeBoxColor s = maybe (Just Yellow) (boxColor s)

-- unsolvedEdges :: Slitherlink -> [Edge]
-- unsolvedEdges = filter (\l -> getLineType l == None) . getLines

isLineType :: Slitherlink -> Maybe LineType -> Line -> Bool
isLineType s typ l = (getLineType s l == typ)

isBoxColor :: Slitherlink -> Maybe BoxColor -> Box -> Bool
isBoxColor s col b = (boxColor s b == col)

setLinesTo :: LineType -> [Line] -> Slither.Updates
setLinesTo typ = updateLines . map (\line -> (line, typ))

setBoxesTo :: BoxColor -> [Box] -> Slither.Updates
setBoxesTo col = updateBoxes . map (\box -> (box, col))

oppColor Blue = Yellow
oppColor Yellow = Blue

lineColorRule :: Slitherlink -> Line -> Slither.Updates
lineColorRule s line = let
    lineType = getLineType s line
    (b1, b2) = lineAdjBoxes s line
    in case lineType of
        Nothing -> case (maybeBoxColor s b1, maybeBoxColor s b2) of
            (Nothing, _) -> noUpdates
            (_, Nothing) -> noUpdates
            (Just c1, Just c2) -> if c1 == c2
                then updateLines [(line, X)]
                else updateLines [(line, L)]
        Just X -> case (maybeBoxColor s b1, maybeBoxColor s b2) of
            (Just c1, Nothing) ->
                maybe noUpdates (\b2 -> updateBoxes [(b2, c1)]) b2
            (Nothing, Just c2) ->
                maybe noUpdates (\b1 -> updateBoxes [(b1, c2)]) b1
            (mc1, mc2) -> if mc1 == mc2 then noUpdates
                else error $ (show mc1) ++ (show mc2) ++ "unsolvable slitherlink"
        Just L -> case (maybeBoxColor s b1, maybeBoxColor s b2) of
            (Just c1, Nothing) ->
                maybe noUpdates (\b2 -> updateBoxes [(b2, oppColor c1)]) b2
            (Nothing, Just c2) ->
                maybe noUpdates (\b1 -> updateBoxes [(b1, oppColor c2)]) b1
            (Nothing, Nothing) -> noUpdates
            (mc1, mc2) -> if mc1 /= mc2 then noUpdates
                else error $ (show mc1) ++ (show mc2) ++ "unsolvable slitherlink"

-- make more general!
-- change type to (Maybe Int) I guess
boxLineRule :: Slitherlink -> Box -> Slither.Updates
boxLineRule s box = case (boxNum s box) of
    Nothing -> noUpdates
    Just num -> inferLines s box num
    where
        inferLines s box num = let
            incident = boxIncidentLines s box
            ls = filter (isLineType s $ Just L) incident
            xs = filter (isLineType s $ Just X) incident
            tbds = filter (isLineType s Nothing) incident
            in
                if length tbds + length ls < num || length ls > num
                    then error "boxlinerule: unsolvable slitherlink"
                else if length tbds + length ls == num
                    then setLinesTo L tbds
                else if length ls == num
                    then setLinesTo X tbds
                else noUpdates

-- adjacent 3s rule
-- adjThrees :: Slitherlink -> Box -> Slither.Updates
-- adjThrees s box = case boxNum s box of
--  Just 3 ->

cornerRules :: Slitherlink -> Box -> Slither.Updates
cornerRules s box =
    case (boxNum s box) of
        Nothing -> noUpdates
        Just num ->
            if length (boxAdjBoxes s box) < num
                then updateBoxes [(box, Blue)]
            else if num <
                length (boxIncidentLines s box) - length (boxAdjBoxes s box)
                then updateBoxes [(box, Yellow)]
            else noUpdates

-- line continuing
lineContinueRule :: Slitherlink -> Point -> Slither.Updates
lineContinueRule s p = let
    incident = pointIncidentLines s p
    ls = filter (isLineType s $ Just L) incident
    xs = filter (isLineType s $ Just X) incident
    tbds = filter (isLineType s Nothing) incident
    in
        if length ls == 2
            then setLinesTo X tbds
        else if length ls + length tbds == 2 && length ls > 0
            then setLinesTo L tbds
        else if length ls > 2
            then error "too many lines set"
        else noUpdates

-- 2 coloring rule

colorAdj :: Slitherlink -> Box -> Slither.Updates
colorAdj s box = let
    adj = boxAdjBoxes s box
    numIncident = length $ boxIncidentLines s box
    blue = filter (isBoxColor s $ Just Blue) adj
    yellow = filter (isBoxColor s $ Just Yellow) adj
    idks = filter (isBoxColor s Nothing) adj
    yellows = length yellow + numIncident - length adj
    in case (boxNum s box) of
        Nothing -> noUpdates
        Just num ->
            if length blue >= num
                && length idks + yellows == num
                then setBoxesTo Yellow idks
            else if yellows >= num
                && length idks + length blue == num
                then setBoxesTo Blue idks
            else noUpdates





-- the coloring rules for 1s and 3s also
-- if it has at least 2 neighbors
-- it has to be the same / diff
