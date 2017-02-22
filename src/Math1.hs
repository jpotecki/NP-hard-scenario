{-# LANGUAGE RecordWildCards, ConstraintKinds #-}

module Math1 (intersectSegSeg, intersect, round', isInScope') where

import Geom2D
import Data.Maybe
import Data.List (minimumBy, (\\))
import Types

type Constraint a = (Ord a, Floating a, Show a, RealFrac a)

intersectSegSeg ::  Constraint a => Point a -> Point a -> Point a -> Point a
                --  -> Maybe a 
                -> Maybe (a, Point a)
intersectSegSeg a b c d =
    -- intersect (Line a b) (Line c d) >>= Just . fst
    intersect (Line a b) (Line c d)

intersect ::  Constraint a => Line a -> Line a 
                                --  -> Maybe (a, Line a) 
                                -> Maybe (a, Point a)
intersect l1@(Line start target) l2 = do
    let intersecPoint = lineIntersect l1 l2 0
    case intersecPoint of 
      Nothing -> Nothing
      Just x  -> if isInScope l1 l2 x
                --  then Just (vectorDistance x start, l2)
                 then Just (vectorDistance x start, x)
                 else Nothing
    --   Just x -> Just (vectorDistance x start, l2)

isInScope ::  Constraint a => Line a -> Line a 
          -> Point a -- Intersection Point
          -> Bool
isInScope l1 l2 p = 
    isInScope' p l1 && isInScope' p l2 -- && not (isInScope' t l2)

isInScope' ::  Constraint a => Point a -> Line a -> Bool
isInScope' p (Line p1 p2)
    | p == p1   = True
    | p == p2   = True
    | otherwise = abs( (p1dist + p2dist) - (linedist ) ) < epsilon
    -- P1 ------------- p ------------P2
  where linedist = vectorDistance p1 p2
        p1dist   = vectorDistance p p1
        p2dist   = vectorDistance p p2

round' :: Constraint a => a -> a
round' = \f -> (fromInteger $ round $ f * (10**14)) / (10.0**14)

epsilon :: Constraint a => a
epsilon = 0.000000001