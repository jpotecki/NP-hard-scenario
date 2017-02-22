{-# LANGUAGE RecordWildCards, ConstraintKinds #-}

module Math1 (intersectSegSeg, intersect, round', isInScope') where

import Geom2D
import Data.Maybe
import Data.List (minimumBy, (\\))
import Types

type Constraint a = (Ord a, Floating a, Show a, RealFrac a)

intersectSegSeg ::  Constraint a => Point a -> Point a -> Point a -> Point a
                 -> Maybe a
intersectSegSeg a b c d =
    intersect (Line a b) (Line c d) >>= Just . fst

intersect ::  Constraint a => Line a -> Line a 
                                 -> Maybe (a, Line a) 
intersect l1@(Line start target) l2 = do
    let intersecPoint = lineIntersect l1 l2 0
    case intersecPoint of 
      Nothing -> Nothing
      Just x  -> if isInScope l1 l2 x
                 then Just (vectorDistance x start, l2)
                 else Nothing
    --   Just x -> Just (vectorDistance x start, l2)

isInScope ::  Constraint a => Line a -> Line a 
          -> Point a -- Intersection Point
          -> Bool
isInScope l1 l2 p = 
    isInScope' p l1 && isInScope' p l2 -- && not (isInScope' t l2)

isInScope' ::  Constraint a => Point a -> Line a -> Bool
isInScope' p (Line p1 p2)
    | p == p1   = True
    | p == p2   = True
    | otherwise = abs( (p1dist + p2dist) - (linedist ) ) < epsilon
    -- P1 ------------- p ------------P2
  where linedist = vectorDistance p1 p2
        p1dist   = vectorDistance p p1
        p2dist   = vectorDistance p p2

round' :: Constraint a => a -> a
round' = \f -> (fromInteger $ round $ f * (10**14)) / (10.0**14)

epsilon :: Constraint a => a
epsilon = 0.000000001

-- findMinDist :: (Ord a, Floating a) => [(a, Line a)] -> Line a
-- -- ^ finds the closest line to the point !!! [Line a] cannot be empty list
-- findMinDist ls = snd $ minimumBy minimumP ls
--   where minimumP t1 t2 = (fst t1) `compare` (fst t2)
-- --     -- minimumP :: Ord a => (a, Line a) -> (a, Line a) -> Ordering
    
-- --------------------------------------------------------------------------------
-- getPath :: (Show a, Ord a, Floating a)
--                 => Path a  -- Accumulator
--                 -> Start a -> Target a
--                 -> [Line a] --Obstacles
--                 -> IO (Path a)
-- -- ^ takes two points and returns a paths
-- getPath acc start target obst = do
--     let line          = Line start target
--         intersections = findAllIntersections line obst
--     putStrLn "Intersections"
--     -- mapM_ print intersections
--     case intersections of
--         [] -> return $ join acc $ Path [line] (vectorDistance start target)
--         xs -> do let nextLine = findMinDist xs
--                      pR       = getP1 nextLine
--                      pL       = getP2 nextLine
--                  putStrLn $ "going to " ++ show nextLine
--                  let accL = Path [Line start pL] (vectorDistance start pL)
--                  let accR = Path [Line start pR] (vectorDistance start pR)
--                  right<- getPath' accR pR nextLine
--                  left <- getPath' accL pL nextLine
--                  return $ join acc (min left right)
--                 --  return $ join acc right
--   where getPath' acc p nl | p == start = return EmptyPath
--                           | otherwise  = getPath acc p target (obst \\ [nl])
