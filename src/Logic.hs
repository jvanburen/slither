module Logic (theRules, start3Rules) where

import Slither
import Adj
import qualified Color as Clr
import Data.Maybe (maybe, fromJust, catMaybes, mapMaybe)
import Control.Monad
import Printer

isLineType :: GameState -> Maybe LineType -> Line -> Bool
isLineType gs = flip ((==) . lineType gs)

-- sepColors
--     acts on GameState and doesn't add updates? or does
-- addUpdates
--     acts on GameState also

lineColorRule :: Slither.Rule
lineColorRule (SetLineTo L line) gs = Slither.sepColors c1 c2 gs
    where 
        (c1, c2) = Slither.lineIncidentColors gs line
lineColorRule (SetLineTo X line) gs =
    Slither.joinColors c1 c2 gs
    where 
        (c1, c2) = Slither.lineIncidentColors gs line
lineColorRule _ gs = Just gs

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

-- getOther b (b1, b2) = if b == b1 then b2 else b1

innerBoxLineRule :: Maybe Slither.Box -> GameState -> Maybe GameState
innerBoxLineRule box gs = case box of
    Nothing -> Just gs
    Just box -> case (boxNum gs box) of
        Nothing -> Just gs
        Just num -> let
                incident = toList $ boxIncidentLines box
                lines = filter (isLineType gs $ Just L) incident
                tbds = filter (isLineType gs Nothing) incident
                numLines = length lines
                numTbds = length tbds

                zipped = zip incident $ toList $ boxAdjColors gs box
                tbdszipped = filter (isLineType gs Nothing . fst) zipped
            in
                if numTbds == 0
                    then Just gs
                else if numLines + numTbds < num 
                    then Nothing
                else if numLines + numTbds == num
                    then Slither.setLinesTo L tbds gs
                else if numLines == num
                    then Slither.setLinesTo X tbds gs
                else case tbdszipped of
                    [(l1, c1), (l2, c2)] -> let
                        -- incident2 = zip incident $ toList $ boxAdjColors gs box
                        -- c1 = fromJust $ lookup l1 incident2
                        -- c2 = fromJust $ lookup l2 incident2
                        in Slither.sepColors c1 c2 gs
                    _ -> Just gs

boxLineRule :: Slither.Rule
boxLineRule (SetLineTo _ line) gs =         
    innerBoxLineRule b1 gs >>= innerBoxLineRule b2
    where
        (b1, b2) = lineIncidentBoxes gs line
boxLineRule _ gs = Just gs

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
        incident = catMaybes $ toList $ pointIncidentLines gs p
        ls = filter (isLineType gs $ Just L) incident
        xs = filter (isLineType gs $ Just X) incident
        tbds = filter (isLineType gs Nothing) incident
    in
        case (length ls, tbds) of
            (2, _) ->      Slither.setLinesTo X tbds gs
            (1, [line]) -> Slither.setLineTo L line gs
            (0, [line]) -> Slither.setLineTo X line gs
            (1, _) -> Just gs
            (0, _) -> Just gs
            _ -> Nothing

lineContinueRule :: Slither.Rule
lineContinueRule (SetLineTo _ line) gs = 
    lineContinue p1 gs >>= lineContinue p2
    where (p1, p2) = lineIncidentPoints line 
lineContinueRule _ gs = Just gs


start3Rules :: Slither.Box -> GameState -> Maybe GameState
start3Rules box gs =
    case boxNum gs box of
        Just 3 -> orthogonal3Rule box gs >>= diagonal3Rule box
        Just 0 -> setLinesTo X (boxIncidentLines box) gs
        _ -> Just gs

orthogonal3Rule :: Slither.Box -> GameState -> Maybe GameState
orthogonal3Rule box gs =
    if not $ is3 box then Just gs
    else let
        (Adj ub db lb rb)  = boxAdjBoxes gs box
        (Adj ul dl ll rl) = boxIncidentLines box
        actions = [ (ub, [dl, ul])
                  , (lb, [ll, rl])
                  , (db, [ul])
                  , (rb, [ll])
                  ]
        relevantBoxes = filter (maybe False is3 . fst) actions
    in
        Slither.setLinesTo L (join $ map snd relevantBoxes) gs
    where 
        is3 b = boxNum gs b == Just 3

diagonal3Rule :: Slither.Box -> GameState -> Maybe GameState
diagonal3Rule box gs =
    let
        (Adj up_l down_l left_l right_l) = boxIncidentLines box
        directions = [ (right, up)
                     , (right, down)
                     , (left, up)
                     , (left, down)
                     ]
        corners = [ [left_l, down_l] 
                  , [left_l, up_l]
                  , [right_l, down_l]
                  , [right_l, up_l]
                  ]  -- order corresponds to `directions`
        diagonals = map (moveDiag gs box) directions

        which = zipWith (singleDiagonal gs) directions diagonals
        ls = join $ map snd $ filter fst $ zip which corners
    in
        setLinesTo L ls gs

type MB = Maybe Box

singleDiagonal :: GameState -> (Adj MB -> MB, Adj MB -> MB) -> MB -> Bool
singleDiagonal gs dir Nothing = False
singleDiagonal gs dir (Just box) = 
    case boxNum gs box of
        Just 3 -> True
        Just 2 -> singleDiagonal gs dir (moveDiag gs box dir)
        _ -> False


moveDiag :: GameState -> Box -> (Adj MB -> MB, Adj MB -> MB) -> MB
moveDiag gs box (f1, f2)  = do
    box' <- f1 (boxAdjBoxes gs box)
    f2 (boxAdjBoxes gs box')

theRules = [lineColorRule, boxLineRule, lineContinueRule]


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
-- separateMaxBoxes :: Slitherlink -> Line -> Slither.Updates
-- separateMaxBoxes s line =
--     if isMaxBox s b1 && isMaxBox s b2
--         then updateLines [(line, L)]
--         else noUpdates
--     where (b1, b2) = lineAdjBoxes s line

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
