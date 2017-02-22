{-# LANGUAGE RecordWildCards, ConstraintKinds, BangPatterns #-}

module Math2 (
          intersectWithPoly, intersectSegSeg
        , eitherClosestPointPolygon, eitherCollision, getClosestPoly
        , closestPoint', getNumberOfIntersections, polygonSegs, initGetPath, purePath
        ) where

import Geom2D
import Data.Maybe
import Data.List (minimumBy, (\\), elemIndex, find)
import Types
import Debug.Trace
import Math1 (intersectSegSeg)

type Seg a = (Point a, Point a)
type StepTarget a = Maybe (Point a, Polygon a)
type Origin a = Point a
type Obstacles a = [Polygon a]
type PathAcc a = Path a
type Sitting a = Maybe (Polygon a)
type Constraint a = (Ord a, Floating a, Show a, RealFrac a, Eq a)
type Memory a = Maybe [Point a]

purePath = getPath

initGetPath :: Constraint a
        => Origin a 
        -> Target a 
        -> Obstacles a 
        -> Path a
initGetPath origin target obstacles =
    let sitting = find (isMyPolygon origin) obstacles
     in getPath origin sitting target obstacles EmptyPath Nothing

getPath :: Constraint a
        => Origin a 
        -> Sitting a -- Just Polygon the polygon we are currently sitting on
        -> Target a 
        -> Obstacles a 
        -> PathAcc a
        -> Memory a
        -> Path a
getPath origin sitting target obstacles !acc !memory = do
    case eitherClosestPointPolygon (origin, target) obstacles sitting of
        {- eitherClosestPointPolygon
        Left True   := rotate in the sitting polygon
        Left False  := direct Way exists
        Right point := closest point of the closest polygon which intersects seg -}
        Left False ->
            -- trace ("direct path " ++ show target)
            join acc $ Path [Line origin target] (vectorDistance origin target)
        Left True  ->
            -- trace ("rotate!")
            rotateRight origin sitting target obstacles acc memory
        Right (po, poly) -> do
          if po `isIn` memory 
          then rotateRight origin sitting target obstacles acc memory
          else
            -- let p = trace ("go first to " ++ show po ++ "then to " ++ show target) getPath origin sitting po (obstacles \\ [poly]) acc (origin `putInto` memory)
            let p = getPath origin sitting po (obstacles \\ [poly]) acc memory
             in getPath po (Just poly) target obstacles p (po `putInto` memory)

isIn :: Constraint a => Point a -> Memory a -> Bool
point `isIn` Nothing = False
point `isIn` (Just xs) = point `elem` xs

putInto ::  Constraint a => Point a -> Memory a -> Memory a
p `putInto` Nothing = Just [p]
p `putInto` Just ps = Just (p:ps)

rotateRight :: Constraint a
        => Origin a 
        -> Sitting a -- Just Polygon the polygon we are currently sitting on
        -> Target a 
        -> Obstacles a 
        -> PathAcc a
        -> Memory a
        -> Path a
rotateRight _ Nothing _ _ _ _ = EmptyPath
rotateRight origin sit@(Just (Polygon points)) target obstacles !acc memory =
    let maybeIndex = elemIndex origin points
     in case maybeIndex of
        Nothing    -> EmptyPath
        Just index -> do
            let p    = (cycle points) !! (index + 1) -- get the next point
                acc' = join acc $ Path [Line origin p] (vectorDistance origin p)
             in getPath p sit target obstacles acc' $ p `putInto` memory

getPS :: Polygon a -> [Point a]
getPS (Polygon xs) = xs

tuple2Point :: (Ord a, Floating a) => (a, a) -> Point a
tuple2Point (x, y) = Point x y

polygonSegs :: Polygon a -> [Seg a]
-- ^ transfors a poligon to a list of lines
polygonSegs (Polygon xs) = zipWith ((,)) xs xs'
  where xs' = (last xs) : init xs

intersectWithPoly :: Constraint a => Seg a -> Polygon a 
                --   -> Maybe (a, Polygon a)
                    -> Maybe (a, Point a, Polygon a)
-- ^ returns a tuple (distance, polygon) if the Segment intersects with it
intersectWithPoly seg poly =
    -- intersectWithPoly' seg poly >>= \d -> Just (d, poly)
    intersectWithPoly' seg poly >>= \(d, point) -> Just (d, point, poly)

intersectWithPoly' :: Constraint a 
                  => Seg a -> Polygon a 
                --   -> Maybe a
                    -> Maybe (a, Point a)
-- ^ returns the distance if Segment intersects with it
intersectWithPoly' seg@(origin, _) poly = case intersect of
    [] -> Nothing
    xs -> Just $ minimumBy minimumP xs
    -- xs -> Just $ minimum xs
  where
    minimumP p1 p2 = (fst p1) `compare` (fst p2)
    intersect = mapMaybe (intersectSegSeg' seg) (polygonSegs poly)
    -- intersectSegSeg' ::Constraint a=> Seg a -> Seg a -> Maybe a
    intersectSegSeg' (a,b) (c,d) =
        intersectSegSeg a b c d

closestPoint' :: Constraint a => Point a -> Polygon a -> Point a
closestPoint' p (Polygon ps) = snd $ minimumBy minimumP ps'
    where
      minimumP p1 p2 = (fst p1) `compare` (fst p2)
      ps' = map (\p' -> (vectorDistance p p', p')) ps

isMyPolygon :: Constraint a => Point a -> Polygon a -> Bool
-- ^ True, if point is a vertex of the polygon
isMyPolygon p (Polygon ps) = p `elem` ps 

notMyPresident :: Constraint a => Point a -> Polygon a -> Bool
notMyPresident p ps = not $ isMyPolygon p ps

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
        -- Right polys     -> let poly = fromJust $ getClosestPoly origin polys
                            -- in Right $ (,) (closestPoint' origin poly) poly
        Right polys -> let (p', py) = fromJust $ getClosestPoly origin polys
                        in Right (p',py)


eitherClosestPointPolygon seg@(origin, target) obstacles (Just sitting) =
    case eitherCollision seg obstacles of
        Left _  -> Left False
        -- Right x -> do
        Right x -> do
            let ownColl = head $ filter (==sitting) $ map (\(_,_,p) -> p) x
             in if collisionWithOwnPoly seg ownColl
                then Left True
                else  let filtered = filter (\(_,_, p) -> p /= sitting) x
                          closest  = getClosestPoly origin filtered
                       in case closest of
                            Nothing -> Left False
                            -- Just  y -> Right $ (,) (closestPoint' origin y) y
                            Just (point, polygon) -> Right (point, polygon)



eitherCollision :: Constraint a => Seg a -> Obstacles a 
                -- -> Either Bool [(a, Polygon a)]
                -> Either Bool [(a, Point a, Polygon a)]
eitherCollision seg@(origin, target) obstacles =
    case mapMaybe (intersectWithPoly seg) obstacles of
        [] -> Left False
        xs -> Right xs

getClosestPoly :: Constraint a 
            --    => Point a -> [(a, Polygon a)] -> Maybe (Polygon a)
     => Point a -> [(a, Point a, Polygon a)] -> Maybe (Point a,Polygon a)

getClosestPoly _ [] = Nothing
-- getClosestPoly p list = Just . snd $ minimumBy minimumP list
--   where minimumP p1 p2 = (fst p1) `compare` (fst p2)
getClosestPoly p list = let (_, point, poly) = minimumBy minimumP list
                         in Just (point, poly)
  where minimumP = \(d1,_,_) (d2,_,_) -> d1 `compare` d2

collisionWithOwnPoly :: Constraint a
                     => Seg a -> Polygon a -> Bool
collisionWithOwnPoly seg poly = getNumberOfIntersections seg poly > 2

getNumberOfIntersections :: Constraint a
                         => Seg a -> Polygon a
                         -> Int
getNumberOfIntersections segment polygon = length intersect
  where intersect = mapMaybe (intersectSegSeg' segment) (polygonSegs polygon)
        intersectSegSeg' (a,b) (c,d) = intersectSegSeg a b c d