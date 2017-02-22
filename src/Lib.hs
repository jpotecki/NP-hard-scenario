module Lib
    ( calcPaths
    ) where

import Math2
import Geom2D
import Data.List
import Control.Parallel.Strategies
import Types
import Parsing
import Data.Either
import Debug.Trace
import Math1 (round')
import Numeric

robots :: [DPoint]
robots =  fmap tuple2Point rawRobot

rawRobot :: [(Double, Double)]
rawRobot = [(0,1),(2,0),(3,5),(6,2),(9,0)]

toPolygon :: [(Double, Double)] -> Polygon Double
toPolygon xs = Polygon $ tuple2Point <$> xs

polygons :: [Polygon Double]
polygons = toPolygon <$> rawPolygons'

rawPolygons' :: [ [(Double, Double)] ] 
rawPolygons' =  [ [(1,2),(1,4),(3,4),(3,2)]
                , [(8,1),(4,1),(4,4),(5,2)]
                ]

type Robots = [(Double, Double)]
type Obstacles = [[(Double, Double)]]

getPath = initGetPath

scalar :: Double
scalar = 10**6

normalize :: Double -> Double
normalize x = x / scalar

scale :: Double -> Double
scale x = x * scalar

scaleRobots :: [(Double, Double)] -> [(Double, Double)]
scaleRobots robots = 
  map (\(x, y) -> (scale x, scale y)) robots

scaleObstacles :: [[(Double, Double)]] -> [[(Double, Double)]]
scaleObstacles obst = map (map') obst
  where map' list = map (\(x, y) -> (scale x, scale y)) list

calcPaths :: String -> IO ()
calcPaths xs = case parseLine xs of 
    Left  e     -> print e
    Right (robots,obstacles) -> calculateSolution robots obstacles
                                -- let robots'    = scaleRobots robots
                                --     obstacles' = scaleObstacles obstacles
                                --  in calculateSolution robots' obstacles'

getPath' :: [Polygon Double] -> (Point Double, Point Double) -> Path Double
getPath' obstacles (a, b) = getPath a b obstacles

updateAwaken :: [(Point Double, Point Double)] -> Point Double -> Point Double
updateAwaken [] y = y
updateAwaken (x:xs) y 
    | fst x == y = snd x
    | otherwise = updateAwaken xs y 

nextPaths :: [Point Double] -> [Point Double] -> [Polygon Double] -> [Path Double] -> [Path Double]
-- ^ Calculates the next path for each awaken robot 
nextPaths _ [] _ acc = acc
nextPaths awaken asleep obstacles acc = 
    (nextPaths (awaken' ++ justAwaken) stillAsleep obstacles (newAcc ++ emptyPaths))
    where 
        combinations = zip awaken asleep
        newPaths = map (getPath' obstacles) combinations -- [p1, p2]
        justAwaken = map snd combinations
        stillAsleep = filter (\ x -> not (x `elem` justAwaken)) asleep
        newAcc = zipWith join acc (newPaths ++ (repeat EmptyPath))
        emptyPaths = replicate (length justAwaken) EmptyPath
        awaken' = justAwaken ++ (drop (length justAwaken) awaken) 

calculateSolution :: Robots -> Obstacles -> IO ()
calculateSolution robs obs = do
    -- mapM_ print robs
    let robots'   = map (tuple2Point) robs
        polygons' = toPolygon <$> obs
        solution = nextPaths ([head robots']) (tail robots') polygons' [EmptyPath]
    mapM_ printPath $ filter (\x -> not (x == EmptyPath)) solution

printPath :: Path Double -> IO ()
printPath (Path ls _) = do
  let ps = init ls
      (Line (Point a b) (Point c d)) = last ls
  mapM_ (\(Line (Point a' b') _)->putStr $ concat ["(",show' a',", ",show' b',"),"]) ps
  putStr $ "(" ++ show' a ++ ", " ++ show' b ++ "), (" ++ show' c ++ ", " ++ show' d ++ ");"

show' :: Double -> String
show' d = Numeric.showFFloat Nothing d ""
  where
    d' = normalize d