module Map.Map where

import Map.Types

import Data.Function (on)
import Data.Array    (bounds, (!))
import Data.List     (sort, sortBy, group)

-- WARNING: Does NOT Check for neighbours exceeding maximum map coordinates yet.
unsafeGiveNeighbours :: (Int, Int)  -- ^ original coordinates
                     -> [(Int,Int)] -- ^ list of neighbours
unsafeGiveNeighbours (x,z) = filter (not . negative) allNs
  where
    allNs = if even z then [(x+1,z), (x-1,z), (x,z+1), (x,z-1), (x+1,z+1), (x+1,z-1)]
                      else [(x+1,z), (x-1,z), (x,z+1), (x,z-1), (x-1,z+1), (x-1,z-1)]

    negative :: (Int, Int) -> Bool
    negative (a,b) = a < 0 || b < 0

giveNeighbours :: PlayMap      -- ^ Map on which to find neighbours
               -> (Int, Int)   -- ^ original coordinates
               -> [(Int, Int)] -- ^ list of neighbours
giveNeighbours mp (x,z) = filter (not . outOfBounds mp) allNs
  where
    allNs = if even z then [(x+1,z), (x-1,z), (x,z+1), (x,z-1), (x+1,z+1), (x+1,z-1)]
                      else [(x+1,z), (x-1,z), (x,z+1), (x,z-1), (x-1,z+1), (x-1,z-1)]

    outOfBounds :: PlayMap -> (Int, Int) -> Bool
    outOfBounds mp' (a,b) = let (lo,hi) = bounds mp' in
                            a < fst lo || b < snd lo || a > fst hi || b > snd hi

giveNeighbourhood :: PlayMap      -- ^ map on which to find neighbourhood
                  -> Int          -- ^ iterative
                  -> (Int, Int)   -- ^ original coordinates
                  -> [(Int, Int)] -- ^ neighbourhood
giveNeighbourhood _  0 (a,b) = [(a,b)]
giveNeighbourhood mp n (a,b) = let ns = giveNeighbours mp (a,b) in 
                             remdups . concat $ ns : map (giveNeighbourhood mp (n-1)) ns

-- | Calculates the height of any given point on the map.
 -- Does not add camera distance to ground to that.
-- 
-- This ueses barycentric coordinate stuff. Wanna read more?
-- http://en.wikipedia.org/wiki/Barycentric_coordinate_system_%28mathematics%29
-- http://www.alecjacobson.com/weblog/?p=1596
--
giveMapHeight :: PlayMap
              -> (Float, Float)  -- ^ Coordinates on X/Z-axes 
              -> Float           -- ^ Terrain Height at that position
giveMapHeight mp (x,z)
  | outsideMap (x',z)           = 0.0
  | (isInt z 6) && (isInt x' 6) = hlu (round x', round z)
  | (isInt z 6)                 = let dist_down = x' - fromIntegral ((floor x') :: Int)
                                      dist_up   = fromIntegral ((ceiling x') :: Int) - x'
                                  in  (1 - dist_down) * (hlu (floor x', round z)) + (1 - dist_up) * (hlu (ceiling x', round z))
  |                (isInt x' 6) = let dist_down = z - fromIntegral ((floor z) :: Int)
                                      dist_up   = fromIntegral ((ceiling z) :: Int) - z
                                  in  (1 - dist_down) * (hlu (round x', floor z)) + (1 - dist_up) * (hlu (round x', ceiling z))
  | otherwise                  = let [a,b,c] = getTrianglePoints [tff,tfc,tcf,tcc]
                                     ar = area (fi a) (fi b) (fi c)
                                     λa = area (fi b) (fi c) (x, z) / ar
                                     λb = area (fi a) (fi c) (x, z) / ar
                                     λc = area (fi a) (fi b) (x, z) / ar
                                 in  (λa * hlu a) + (λb * hlu b) + (λc * hlu c)
  where

    -- compensating
    x' = x * ((sqrt 3) / 2)

    --Returns if q is an int to n decimal places
    isInt :: RealFrac b => b -> Int -> Bool
    isInt q n = (round $ 10^((fromIntegral n) :: Integer) * (q - (fromIntegral ((round q):: Integer)))) == (0 :: Integer)

    outsideMap :: (Float, Float) -> Bool
    outsideMap (mx, mz) = let ((a,b),(c,d)) = bounds mp
                              fr = fromIntegral
                          in  mx < (fr a) || mx > (fr c) || mz < (fr b) || mz > (fr d)
 
    fi :: (Int, Int) -> (Float, Float)
    fi (m, n) = (fromIntegral m, fromIntegral n)

    -- Height LookUp
    hlu :: (Int, Int) -> Float
    hlu (k,j) = let (Node _ (_,_,y) _ _ _ _ _ _) = mp ! (k,j) in y

    ff  = (floor   x, floor   z) :: (Int, Int)
    fc  = (floor   x, ceiling z) :: (Int, Int)
    cf  = (ceiling x, floor   z) :: (Int, Int)
    cc  = (ceiling x, ceiling z) :: (Int, Int)

    tff = (ff, dist (x,z) ff)
    tfc = (fc, dist (x,z) fc)
    tcf = (cf, dist (x,z) cf)
    tcc = (cc, dist (x,z) cc)

    getTrianglePoints :: [((Int,Int), Float)] -> [(Int,Int)]
    getTrianglePoints = ((take 3) . (map fst) . (sortBy (compare `on` snd)))

    dist :: (Float, Float) -> (Int, Int) -> Float
    dist (x1,z1) (x2,z2) = let xf = x1 - fromIntegral x2
                               zf = z1 - fromIntegral z2
                           in  sqrt $ xf*xf + zf*zf

    -- Heron's Formula: http://en.wikipedia.org/wiki/Heron%27s_formula
    area :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Float
    area (x1,z1) (x2,z2) (x3,z3) = let a = sqrt $ (x1-x2)*(x1-x2) + (z1-z2)*(z1-z2)
                                       b = sqrt $ (x2-x3)*(x2-x3) + (z2-z3)*(z2-z3)
                                       c = sqrt $ (x1-x3)*(x1-x3) + (z1-z3)*(z1-z3)
                                       s = (a+b+c)/2 
                                   in  sqrt $ s * (s-a) * (s-b) * (s-c)

-- removing duplicates in O(n log n), losing order and adding Ord requirement
remdups :: Ord a => [a] -> [a]
remdups = map head . group . sort
