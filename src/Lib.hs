module Lib
    ( someFunc
    ) where

import Math1
import Geom2D
import Data.List
import Control.Parallel.Strategies
import Types

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

someFunc :: IO ()
someFunc = let  robotPermutations = (,) <$> init robots <*> tail robots
                polygons'         = getAllObstacleLines polygons
                getPath' = \(r1,r2) -> getPath EmptyPath r1 r2 polygons'
                mapping  = map getPath' robotPermutations -- lazy piece of shit
                computed = mapping `using` parList rseq
             in mapM_ (print . normalizePath) computed