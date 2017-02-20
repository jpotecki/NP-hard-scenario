{-# LANGUAGE RecordWildCards #-}
module Math1 where

import Geom2D
import Data.Maybe
import Data.List (minimumBy, (\\))

type Start a  = Point a
type Target a = Point a

data Path a
   = Path
   { path :: [Line a]
   , dist :: a
   } 
   | EmptyPath
   deriving (Show, Eq)

instance Ord a => Ord (Path a) where
    compare EmptyPath EmptyPath = EQ
    compare EmptyPath Path{..}   = LT
    compare Path{..} EmptyPath   = GT
    compare Path{dist = d1} Path{dist = d2} = d1 `compare` d2

join :: (Floating a) => Path a -> Path a -> Path a
join p1 EmptyPath = p1
join EmptyPath p2 = p2
join p1 p2 = Path (path p1 ++ path p2) (dist p1 + dist p2)

getP1 :: Line a -> Point a
getP1 (Line p _) = p

getP2 :: Line a -> Point a 
getP2 (Line _ p) = p

tuple2Point :: (Ord a, Floating a) => (a, a) -> Point a
tuple2Point (x, y) = Point x y

transformPolygon :: Polygon a -> [Line a]
-- ^ transfors a poligon to a list of lines
transformPolygon (Polygon xs) = do
    zipWith (Line) xs xs'
  where
    xs' = (last xs) : init xs

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
        Just x  -> if (isInScope x l2)&&(isInScope x l1)&&not(isInScope target l2)
                   then Just (vectorDistance x start, l2)
                   else Nothing

isInScope :: (Ord a, Floating a) => Point a -> Line a -> Bool
isInScope p (Line p1 p2) = 
        linedist == (vectorDistance p p1) + (vectorDistance p p2) 
  where linedist = vectorDistance p1 p2

findMinDist :: (Ord a, Floating a) => [(a, Line a)] -> Line a
-- ^ finds the closest line to the point !!! [Line a] cannot be empty list
findMinDist ls = snd $ minimumBy minimumP ls
  where
--     -- minimumP :: Ord a => (a, Line a) -> (a, Line a) -> Ordering
    minimumP t1 t2 = (fst t1) `compare` (fst t2)
--     -- distances :: [(a, Line a)]
    -- distances      = map distance ls
--     -- distance :: Line a -> (a, Line a)
    -- distance line  = (lineDistance line p, line)
--------------------------------------------------------------------------------
getPath :: (Show a, Ord a, Floating a) => Path a  -- Accumulator
                               -> Start a-> Target a -> [Line a] 
                               -> IO (Path a)  -- Result
-- ^ takes two points and returns a paths
getPath acc start target obst = do
    let line          = Line start target
        intersections = findAllIntersections line obst
    putStrLn "printing intersections"
    mapM print intersections
    putStrLn "------------------------------------"
    print acc 
    putStrLn "------------------------------------"

    case intersections of
        [] -> return $ join acc $ Path [line] (vectorDistance start target)
        xs -> do
            let acc' =  join acc $ Path [line] (vectorDistance start target)
            let nextLine = findMinDist xs
            putStr "going to point:" 
            print $ getP2 nextLine
            -- right <-getPath acc (getP1 nextLine) target (obst \\ [nextLine])
            left  <- getPath acc' (getP2 nextLine) target (obst \\ [nextLine])
            -- return $ join acc $ min left right
            return $ join acc left