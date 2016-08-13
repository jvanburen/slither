module Solver where

import Slither

import Logic

-- collectUpdates :: [Updates] -> Updates
-- collectUpdates ups = (concat l, concat b)
--     where (l, b) = unzip ups


-- applyLineRule :: (Slitherlink -> Line -> Updates) -> Slitherlink -> Updates
-- applyLineRule f s = collectUpdates $ map (f s) $ getLines s

-- applyPointRule :: (Slitherlink -> Point -> Updates) -> Slitherlink -> Updates
-- applyPointRule f s = collectUpdates $ map (f s) $ getPoints s

-- applyBoxRule :: (Slitherlink -> Box -> Updates) -> Slitherlink -> Updates
-- applyBoxRule f s = collectUpdates $ map (f s) $ getBoxes s

-- applyAllRulesSoFar :: Slitherlink -> Updates
-- applyAllRulesSoFar s = collectUpdates $ map ($ s) [ applyLineRule lineColorRule
--                                                   , applyBoxRule boxLineRule
--                                                   , applyBoxRule cornerRules
--                                                   , applyPointRule lineContinueRule
--                                                   , applyBoxRule colorAdj
--                                                   , applyLineRule separateMaxBoxes
--                                                   ]

