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
import Text.Parsec
import Text.Parsec.String

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

calcPaths :: String -> IO ()
calcPaths xs = case parseLine xs of 
                    Left  e     -> print e
                    Right (r,o) -> do
                        putStrLn "robots"
                        print r
                        putStrLn "obst"
                        print o
                        putStrLn "lines"
                        print $ getAllObstacleLines $ toPolygon <$> o
                        putStrLn "path"
                        
                        calcPath r o

calcPath :: Robots -> Obstacles -> IO ()
calcPath rob obs = do
    let robots' = map (tuple2Point) rob
        robotPermutations = (,) <$> init robots' <*> tail robots'
        polygons'= getAllObstacleLines $ toPolygon <$> obs
        getPath' = \(r1,r2) -> getPath EmptyPath r1 r2 polygons'
    putStrLn "polygon lines"
    print polygons'
    mapping  <- mapM getPath' robotPermutations --lazy piece of s..
        -- computed = mapping `using` parList rseq
    mapM_ (print . normalizePath) mapping
    -- mapM_ print mapping