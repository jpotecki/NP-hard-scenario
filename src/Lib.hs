module Lib
    ( calcPaths
    ) where

import Math1
import Geom2D
import Data.List
import Control.Parallel.Strategies
import Types
import Parsing
import Data.Either

robots :: [(Double,Double)] -> [Point Double]
robots robots =  fmap tuple2Point robots

toPolygon :: [(Double, Double)] -> Polygon Double
toPolygon xs = Polygon $ tuple2Point <$> xs

type Robots = [(Double, Double)]
type Obstacles = [[(Double, Double)]]


calcPaths :: String -> IO ()
calcPaths xs = case parseLine xs of 
                    Left  e     -> print e
                    Right (robots,obstacles) -> calculateSolution robots obstacles

getPath' :: [Line Double] -> (Point Double, Point Double) -> Path Double
getPath' obstacles (a, b) = getPath EmptyPath a b obstacles

updateAwaken :: [(Point Double, Point Double)] -> Point Double -> Point Double
updateAwaken [] y = y
updateAwaken (x:xs) y 
    | fst x == y = snd x
    | otherwise = updateAwaken xs y 

nextPaths :: [Point Double] -> [Point Double] -> [Line Double] -> [Path Double] -> [Path Double]
-- ^ Calculates the next path for each awaken robot 
nextPaths _ [] _ acc = acc
nextPaths awaken asleep obstacles acc = 
    nextPaths (awaken' ++ justAwaken) stillAsleep obstacles (newAcc ++ emptyPaths)
    where 
        combinations = zip awaken asleep
        newPaths = map (getPath' obstacles) combinations
        justAwaken = map snd combinations
        stillAsleep = filter (\ x -> not (x `elem` justAwaken)) asleep
        newAcc = zipWith (join) acc newPaths
        emptyPaths = replicate (length justAwaken) EmptyPath
        awaken' = justAwaken ++ (drop (length justAwaken) awaken) 

calculateSolution :: Robots -> Obstacles -> IO ()
calculateSolution robs obs = do
    let robots'   = map (tuple2Point) robs
        polygons' = getAllObstacleLines $ toPolygon <$> obs
        solution = nextPaths ([head robots']) (tail robots') polygons' [EmptyPath]
    mapM_ print $ filter (\x -> not (x == EmptyPath)) solution



