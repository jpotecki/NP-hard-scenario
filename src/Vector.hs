
module Vector 
    (intersectSegSeg) where

import Types
import Geom2D
import Data.Maybe

--funcs taken and adjusted from 
--hackage.haskell.org/package/gloss-1.10.2.5/docs/src/Graphics-Gloss-Geometry-Line.html
{-# INLINE closestPointOnLineParam #-}
closestPointOnLineParam
        :: (Ord a, Floating a)
        => Point a       -- ^ `P1`
        -> Point a       -- ^ `P2`
        -> Point a       -- ^ `P3`
        -> a

closestPointOnLineParam p1 p2 p3
        = (p3 ^-^ p1) `vectorCross` (p2 ^-^ p1) 
        / (p2 ^-^ p1) `vectorCross` (p2 ^-^ p1)


intersectLineLine
        :: (Ord a, Floating a)
        => Point a        -- ^ `P1`
        -> Point a        -- ^ `P2`
        -> Point a        -- ^ `P3`
        -> Point a        -- ^ `P4`
        -> Maybe (Point a)

intersectLineLine (Point x1 y1) (Point x2 y2) (Point x3 y3) (Point x4 y4)
 = let  dx12    = x1 - x2
        dx34    = x3 - x4

        dy12    = y1 - y2
        dy34    = y3 - y4
        
        den     = dx12 * dy34  - dy12 * dx34

   in if den == 0
        then Nothing
        else let
                det12   = x1*y2 - y1*x2
                det34   = x3*y4 - y3*x4 

                numx    = det12 * dx34 - dx12 * det34
                numy    = det12 * dy34 - dy12 * det34
             in Just $ Point (numx / den) (numy / den)

-- | Get the point where a segment @P1-P2@ crosses another segement @P3-P4@,
--   if any.
intersectSegSeg
        :: (Ord a, Floating a)
        => Point a        -- ^ `P1`
        -> Point a        -- ^ `P2`
        -> Point a        -- ^ `P3`
        -> Point a        -- ^ `P4`
        -> Maybe (Point a)

intersectSegSeg p1 p2 p3 p4
        -- TODO: merge closest point checks with intersection, reuse subterms.
        | Just p0       <- intersectLineLine p1 p2 p3 p4
        , t12           <- closestPointOnLineParam p1 p2 p0
        , t23           <- closestPointOnLineParam p3 p4 p0
        , t12 >= 0 && t12 <= 1
        , t23 >= 0 && t23 <= 1
        = Just p0
        | otherwise
        = Nothing