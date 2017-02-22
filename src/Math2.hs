{-# LANGUAGE RecordWildCards, ConstraintKinds #-}

module Math2 (
          existsDirectWay, intersectWithPoly, intersectSegSeg
        , eitherClosestPointPolygon, eitherCollision, getClosestPoly
        , closestPoint', getNumberOfIntersections, polygonSegs, initGetPath
        ) where

import Geom2D
import Data.Maybe
import Data.List (minimumBy, (\\), elemIndex, find)
import Types
-- import Vector (intersectSegSeg)
import Math1 (intersectSegSeg)

type Seg a = (Point a, Point a)
type StepTarget a = Maybe (Point a, Polygon a)
type Origin a = Point a
type Obstacles a = [Polygon a]
type PathAcc a = Path a
type Sitting a = Maybe (Polygon a)
type Constraint a = (Ord a, Floating a, Show a, RealFrac a)

initGetPath :: Constraint a
        => Origin a 
        -> Target a 
        -> Obstacles a 
        -> Path a
initGetPath origin target obstacles =
    let sitting = find (isMyPolygon origin) obstacles
     in getPath origin sitting target obstacles EmptyPath

getPath :: Constraint a
        => Origin a 
        -> Sitting a -- Just Polygon the polygon we are currently sitting on
        -> Target a 
        -> Obstacles a 
        -> PathAcc a
        -> Path a
getPath origin sitting target obstacles acc = 
  case eitherClosestPointPolygon (origin, target) obstacles sitting of
    Left False -> 
        join acc $ Path [Line origin target] (vectorDistance origin target)
    Left True  -> 
        rotateRight origin sitting target obstacles acc
    Right (point, polygon) ->
        let acc' = join acc $ Path [Line origin point] (vectorDistance origin point)
         in getPath point (Just polygon) target obstacles acc'


dropPolygon' :: (Eq a, Eq b) =>[(b, a)] -> a -> [(b, a)]
-- ^ the item out , which is providied
dropPolygon' [] _ = []
dropPolygon' ps p = filter (\(_,a) -> a /= p) ps

rotateRight :: Constraint a
        => Origin a 
        -> Sitting a -- Just Polygon the polygon we are currently sitting on
        -> Target a 
        -> Obstacles a 
        -> PathAcc a
        -> Path a
rotateRight origin Nothing target obstacles acc = EmptyPath
rotateRight origin sit@(Just (Polygon points)) target obstacles acc =
    let maybeIndex = elemIndex origin points
     in case maybeIndex of
        Nothing    -> EmptyPath
        Just index -> do
            let p    = (cycle points) !! (index + 1) -- get the next point
                acc' = join acc $ Path [Line origin p] (vectorDistance origin p)
             in getPath p sit target obstacles acc'

needToRotate :: Constraint a => Sitting a -> [Polygon a] -> Bool
-- ^ Takes a Polygon and a list of collisions and checks, whether the point needs to rotate
needToRotate Nothing _ = False
needToRotate (Just poly) ps = numberOfCol > 2
  where numberOfCol = length $ filter (== poly) ps

getPS :: Polygon a -> [Point a]
getPS (Polygon xs) = xs

tuple2Point :: (Ord a, Floating a) => (a, a) -> Point a
tuple2Point (x, y) = Point x y

polygonSegs :: Polygon a -> [Seg a]
-- ^ transfors a poligon to a list of lines
polygonSegs (Polygon xs) = zipWith ((,)) xs xs'
  where xs' = (last xs) : init xs

intersectWithPoly :: Constraint a => Seg a -> Polygon a 
                  -> Maybe (a, Polygon a)
-- ^ returns a tuple (distance, polygon) if the Segment intersects with it
intersectWithPoly seg poly =
    intersectWithPoly' seg poly >>= \d -> Just (d, poly)

intersectWithPoly' :: Constraint a 
                  => Seg a -> Polygon a 
                  -> Maybe a
-- ^ returns the distance if Segment intersects with it
intersectWithPoly' seg@(origin, _) poly = case intersect of
    [] -> Nothing
    xs -> Just $ minimum xs
  where
    minimumP p1 p2 = (fst p1) `compare` (fst p2)
    intersect = mapMaybe (intersectSegSeg' seg) (polygonSegs poly)
    -- intersectSegSeg' ::Constraint a=> Seg a -> Seg a -> Maybe a
    intersectSegSeg' (a,b) (c,d) =
        intersectSegSeg a b c d

-- getClosestP :: Constraint a
--             => Point a 
--             -> [Polygon a]
--             -> Point a
-- -- ^ returns the closest vertex to the point
-- getClosestP point polygons = closestPoint' point points'
--   where points' = filter (/= point) $ concat $ map getPS polygons

closestPoint' :: Constraint a => Point a -> Polygon a -> Point a
closestPoint' p (Polygon ps) = snd $ minimumBy minimumP ps'
    where
      minimumP p1 p2 = (fst p1) `compare` (fst p2)
      ps' = map (\p' -> (vectorDistance p p', p')) ps

isMyPolygon :: Constraint a => Point a -> Polygon a -> Bool
-- ^ True, if point is a vertex of the polygon
isMyPolygon p (Polygon ps) = p `elem` ps 

existsDirectWay' :: Constraint a => Point a 
                                 -> StepTarget a 
                                 -> Obstacles a 
                                 -> Bool
existsDirectWay' _ Nothing _ = False
existsDirectWay' origin (Just (p,_)) obst = existsDirectWay (origin, p) obst

existsDirectWay :: Constraint a => Seg a -> Obstacles a -> Bool
existsDirectWay seg obstacles =
    null $ catMaybes $ map (intersectWithPoly seg) obstacles

type Rotate = Bool

eitherClosestPointPolygon :: Constraint a
                          => Seg a -> Obstacles a -> Sitting a
                          -> Either Rotate ((Point a, Polygon a))
{- Left True   := rotate in the sitting polygon
   Left False  := direct Way exists
   Right point := closest point of the closest polygon which intersects the seg
-}
eitherClosestPointPolygon seg@(origin, target) obstacles Nothing =
    case eitherCollision seg obstacles of 
        Left  _         -> Left False
        Right polys     -> let poly = fromJust $ getClosestPoly origin polys
                            in Right $ (,) (closestPoint' origin poly) poly
eitherClosestPointPolygon seg@(origin, target) obstacles (Just sitting) =
    case eitherCollision seg obstacles of
        Left _  -> Left False
        Right x -> do
            let ownColl = head $ filter (==sitting) $ map snd x
             in if collisionWithOwnPoly seg ownColl
                then Left True
                else  let filtered = filter (\(_, p) -> p /= sitting) x
                          closest  = getClosestPoly origin filtered
                       in case closest of
                            Nothing -> Left False
                            Just  y -> Right $ (,) (closestPoint' origin y) y

eitherCollision :: Constraint a => Seg a -> Obstacles a 
                -> Either Bool [(a, Polygon a)]
eitherCollision seg@(origin, target) obstacles =
    case mapMaybe (intersectWithPoly seg) obstacles of
        [] -> Left False
        xs -> Right xs

getClosestPoly :: Constraint a 
               => Point a -> [(a, Polygon a)] -> Maybe (Polygon a)
getClosestPoly _ [] = Nothing
getClosestPoly p list = Just . snd $ minimumBy minimumP list
  where minimumP p1 p2 = (fst p1) `compare` (fst p2)

collisionWithOwnPoly :: Constraint a
                     => Seg a -> Polygon a -> Bool
collisionWithOwnPoly seg poly = getNumberOfIntersections seg poly > 2

getNumberOfIntersections :: Constraint a
                         => Seg a -> Polygon a
                         -> Int
getNumberOfIntersections segment polygon = length intersect
  where intersect = mapMaybe (intersectSegSeg' segment) (polygonSegs polygon)
        intersectSegSeg' (a,b) (c,d) = intersectSegSeg a b c d


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
