{-# LANGUAGE RecordWildCards, ConstraintKinds #-}

module Math2 where

import Geom2D
import Data.Maybe
import Data.List (minimumBy, (\\))
import Types
import Vector (intersectSegSeg)

type Seg a = (Point a, Point a)
type StepTarget a = Point a
type Origin a = Point a
type Obstacles a = [Polygon a]
type PathAcc a = Path a

type Constraint a = (Ord a, Floating a, Show a)

getPath :: Constraint a
        => Origin a 
        -> StepTarget a 
        -> Target a 
        -> Obstacles a 
        -> PathAcc a
        -> Path a
getPath origin stepTarget target obstacles acc =
    if existsDirectWay (origin, target) obstacles
    then join acc $ Path [Line origin target] (vectorDistance origin target)
    else do 
        let seg         = (origin, stepTarget)
            collisions' = catMaybes $ map (intersectWithPoly seg) obstacles
        EmptyPath

getPS :: Polygon a -> [Point a]
getPS (Polygon xs) = xs

tuple2Point :: (Ord a, Floating a) => (a, a) -> Point a
tuple2Point (x, y) = Point x y

polygonSegs :: Polygon a -> [Seg a]
-- ^ transfors a poligon to a list of lines
polygonSegs (Polygon xs) = zipWith ((,)) xs xs'
  where xs' = (last xs) : init xs

intersectWithPoly :: Constraint a 
                  => Seg a -> Polygon a 
                  -> Maybe (a, Polygon a)
-- ^ returns Polygon and the distance if Segment intersects with it
intersectWithPoly seg@(origin, _) poly = case intersect of
    [] -> Nothing
    xs -> Just (minimum xs, poly)
  where 
    minimumP p1 p2 = (fst p1) `compare` (fst p2)
    intersect = mapMaybe (intersectSegSeg' seg) (polygonSegs poly)
    -- intersectSegSeg' ::Constraint a=> Seg a -> Seg a -> Maybe a
    intersectSegSeg' (a,b) (c,d) = 
        intersectSegSeg a b c d >>= \p -> Just $ vectorDistance origin p

getClosestP :: Constraint a
            => Point a 
            -> [Polygon a]
            -> Point a
-- ^ returns the closest vertex to the point
getClosestP point polygons = closestPoint' point points'
  where points' = filter (/= point) $ concat $ map getPS polygons

closestPoint' :: Constraint a => Point a -> [Point a] -> Point a
closestPoint' p ps = snd $ minimumBy minimumP ps'
    where
    minimumP p1 p2 = (fst p1) `compare` (fst p2)
    ps' = map (\p' -> (vectorDistance p p', p)) ps

isMyPolygon :: Constraint a => Point a -> Polygon a -> Bool
-- ^ True, if point is a vertex of the polygon
isMyPolygon p (Polygon ps) = p `elem` ps 

existsDirectWay :: Constraint a => Seg a -> Obstacles a -> Bool
existsDirectWay seg obstacles =
    null $ catMaybes $ map (intersectWithPoly seg) obstacles

deleteFrom :: Constraint a => Point a -> [Polygon a] -> [Polygon a]
point `deleteFrom` polygons = filter notIn polygons
  where notIn = \(Polygon ps) -> point `elem` ps

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
