module Types where 

-- type Start = Point
-- type End   = Point

-- data Point = Point { x :: Double
--                    , y :: Double 
--                    } deriving (Show, Eq)

-- data Vector = VecL { p1 :: Point
--                    , p2 :: Point } deriving (Show, Eq)

-- data Path = Path { start :: Point
--                  , end   :: Point 
--                  , dist  :: Double
--                  , path  :: [Point]
--                  } deriving (Eq, Show)

-- data Line = Line { a :: Double
--                  , b :: Double
--                  , c :: Double } deriving (Eq, Show)

-- type Polygon = [Point]

-- instance Ord Path where
--     compare Path{dist = p1} Path{dist = p2} = p1 `compare` p2

-- -- createPoly :: [Point] -> [Vector]
-- -- createPoly

-- createLine :: Point -> Point -> Line
-- createLine p1 p2 = Line a b (-c)
--   where a = (y p1) - (y p2)
--         b = (x p2) - (x p1)
--         c = (x p1) * (y p2) - (x p2) * (y p1)

-- intersect :: Vector -> Vector -> Bool
-- -- ^ True if 2 vectors intersect
-- -- https://stackoverflow.com/questions/20677795/how-do-i-compute-the-intersection-point-of-two-lines-in-python
-- intersect v1 v2 =
--     let d  = (a l1) * (b l2) - (b l1) * (a l2)
--         dx = (c l1) * (b l2) - (b l1) * (c l2)
--         dy = (a l1) * (c l2) - (c l1) * (a l2)
--     in d /= 0.0
--   where 
--     l1 = createLine (p1 v1) (p2 v1) :: Line
--     l2 = createLine (p1 v2) (p2 v2) :: Line


