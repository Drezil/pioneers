module Map.Map where

import Map.Types
import Map.Graphics (unitLength)

import Data.Array    (bounds, (!))
import Data.List     (sort, group)

import Debug.Trace

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
giveMapHeight :: PlayMap
             -> (Double, Double)
             -> Double
giveMapHeight mop (x, z)
  | outsideMap (x/unitLength,z'/unitLength) = 0.0
  | otherwise         = height' --sum $ map (\(p,d) -> hlu p * (d / totald)) tups
  where
    z' = z * 2/ sqrt 3
    rx = (x/unitLength)  - (fromIntegral $ floor (x/unitLength ))
    rz = (z'/unitLength) - (fromIntegral $ floor (z'/unitLength))

    hoi = map (hlu . clmp . tadd (floor (x/unitLength), floor (z'/unitLength))) mods
      where
        mods = [(0,0),(0,1),(1,0),(1,1)]
        tadd (a,b) (c,d) = (a+c,b+d)

    height' = height*unitLength

    height = --trace (show [rx,rz] ++ show hoi)
             (1-rz) * ((1-rx) * (hoi !! 0) + rx * (hoi !! 2))
           + rz     * ((1-rx) * (hoi !! 1) + rx * (hoi !! 3))

    outsideMap :: (Double, Double) -> Bool
    outsideMap (mx, mz) = let ((a,b),(c,d)) = bounds mop
                              fr = fromIntegral
                          in  mx < fr a || mx > fr c || mz < fr b || mz > fr d

    -- Height LookUp on PlayMap
    hlu :: (Int, Int) -> Double
    hlu (k,j) = let (Node _ (_,_,y) _ _ _ _ _ _) = mop ! (k,j) in y

    -- reference Points
    refs :: [(Int, Int)]
    refs = remdups $ map (clmp . tadd (floor x, floor z')) mods
      where
        mods = [(-1,-1),(-1,2),(0,0),(0,1),(1,0),(1,1),(2,-1),(2,2)]
        tadd (a,b) (c,d) = (a+c,b+d)

    -- tupels with reference point and distance
    tups = zip refs weights --map (\t -> (t, dist (x,z') t)) refs
      where
        weights = [1,2,1,2,4,2,1,2,1]

    -- total distance of all for reference point from the point in question
    totald = sum $ map snd tups

    -- clamp, as she is programmed
    clamp :: (Ord a) => a -> a -> a -> a
    clamp mn mx = max mn . min mx

    -- clamp for tupels
    clmp :: (Int, Int) -> (Int, Int)
    clmp (a,b) = let ((xmin,zmin),(xmax,zmax)) = bounds mop
                 in  (clamp (xmin+2) (xmax-2) a,clamp (zmin+2) (zmax-2) b)

    -- Real distance on PlayMap
    dist :: (Double, Double) -> (Int, Int) -> Double
    dist (x1,z1) pmp = let xf = x1 - realx
                           zf = z1 - realz
                       in  sqrt $ xf*xf + zf*zf
      where
        realx = (\(Node _ (nx,_,_) _ _ _ _ _ _) -> nx) (mop ! pmp)
        realz = (\(Node _ (_,nz,_) _ _ _ _ _ _) -> nz) (mop ! pmp)

-- removing duplicates in O(n log n), losing order and adding Ord requirement
remdups :: Ord a => [a] -> [a]
remdups = map head . group . sort
