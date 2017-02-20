{-# LANGUAGE RecordWildCards #-}

module Types where

import Geom2D

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
    compare EmptyPath EmptyPath  = EQ
    compare EmptyPath Path{..}   = LT
    compare Path{..} EmptyPath   = GT
    compare Path{dist = d1} Path{dist = d2} = d1 `compare` d2

join :: (Floating a) => Path a -> Path a -> Path a
join p1 EmptyPath = p1
join EmptyPath p2 = p2
join p1 p2 = Path (path p1 ++ path p2) (dist p1 + dist p2)

normalizePath :: Eq a => Path a -> Path a
-- ^ If two consequtive points are the same, throw them out
normalizePath Path{..} = Path path' dist
  where
    path' = filter (\(Line x y) -> if x == y then False else True) path

getP1 :: Line a -> Point a
getP1 (Line p _) = p

getP2 :: Line a -> Point a 
getP2 (Line _ p) = p
