{-# LANGUAGE RecordWildCards #-}

module Math1 where

import Geom2D
import Data.Maybe
import Data.List (minimumBy, (\\))
import Types

tuple2Point :: (Ord a, Floating a) => (a, a) -> Point a
tuple2Point (x, y) = Point x y

transformPolygon :: Polygon a -> [Line a]
-- ^ transfors a poligon to a list of lines
transformPolygon (Polygon xs) = zipWith (Line) xs xs'
  where xs' = (last xs) : init xs

getAllObstacleLines :: [Polygon a] -> [Line a]
-- ^ Transforms polygons to a list of lines
getAllObstacleLines ps = concat $ transformPolygon <$> ps

findAllIntersections :: (Ord a, Floating a) => Line a -> [Line a] 
                                            -> [(a,Line a)]
-- ^ returns takes a line `x` and [list] `xs` and returns `xs'` intersecting x
findAllIntersections l1@(Line start _) ls = 
    catMaybes $ map (\x -> intersect l1 x) ls

intersect :: (Ord a, Floating a) => Line a -> Line a 
                                 -> Maybe (a, Line a) -- (distance, line)
intersect l1@(Line start target) l2 = do
    let intersecPoint = lineIntersect l2 l1 0
    case intersecPoint of 
      Nothing -> Nothing
      Just x  -> if isInScope l1 l2 target x
                 then Just (vectorDistance x start, l2)
                 else Nothing

isInScope :: (Ord a, Floating a) => Line a -> Line a 
                                 -> Point a -- Target point 
                                 -> Point a -- Intersection Point
                                 -> Bool
isInScope moveLine@(Line p1 p2) obSide target collisionPoint = 
    pointInLine collisionPoint obSide && pointInLine collisionPoint moveLine && (collisionPoint /= p1)

pointInLine :: (Ord a, Floating a) => Point a -> Line a -> Bool
pointInLine p (Line p1 p2) = 
        linedist == (vectorDistance p p1) + (vectorDistance p p2) 
  where linedist = vectorDistance p1 p2

findMinDist :: (Ord a, Floating a) => [(a, Line a)] -> Line a
-- ^ finds the closest line to the point !!! [Line a] cannot be empty list
findMinDist ls = snd $ minimumBy minimumP ls
  where
--     -- minimumP :: Ord a => (a, Line a) -> (a, Line a) -> Ordering
    minimumP t1 t2 = (fst t1) `compare` (fst t2)
--------------------------------------------------------------------------------
getPath :: (Show a, Ord a, Floating a) => Path a  -- Accumulator
                               -> Start a -> Target a -> [Line a] 
                               -> Path a
-- ^ takes two points and returns a paths
getPath acc start target obst =
    let line          = Line start target
        intersections = findAllIntersections line obst
     in case intersections of
        [] -> join acc $ Path [line] (vectorDistance start target)
        xs ->    let nextLine = findMinDist xs
                     pR       = getP1 nextLine
                     pL       = getP2 nextLine
                     accL  = Path [Line start pL] (vectorDistance start pL)
                     accR  = Path [Line start pR] (vectorDistance start pR)
                     right = getPath accR pR target (obst \\ [nextLine])
                     left  = getPath accL pL target (obst \\ [nextLine])
                  in join acc (min left right)
