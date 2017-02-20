module Lib
    ( someFunc
    ) where

import Math1
import Geom2D

robots :: [Point Double]
robots =  fmap tuple2Point rawRobot

rawRobot :: [(Double, Double)]
rawRobot = (-1,-1):(4,4):[]

toPolygon :: [(Double, Double)] -> Polygon Double
toPolygon xs = Polygon $ tuple2Point <$> xs



polygons :: [Polygon Double]
polygons = toPolygon <$> rawPolygons'

rawPolygons' :: [ [(Double, Double)] ] 
rawPolygons' =  [ [(1,2),(1,4),(3,4),(3,2) ]
                , [(8,1),(4,1),(4,4),(5,2) ]
                ]

someFunc :: IO ()
someFunc = do
    let start  = robots !! 0
        target = robots !! 1
    putStrLn "Printing polygons"
    -- print $ (toPolygon) <$> rawPolygons'
    print  $ getAllObstacleLines polygons
    putStrLn "done printing"
    path   <- getPath EmptyPath start target (getAllObstacleLines polygons)
    putStrLn "Result:"
    print path

