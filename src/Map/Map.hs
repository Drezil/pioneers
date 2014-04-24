module Map.Map where

import Map.Types

import Data.Array (bounds)
import Data.List  (sort, group)

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
                             remdups . concat $ ns:(map (giveNeighbourhood mp (n-1)) ns)

-- removing duplicates in O(n log n), losing order and adding Ord requirement
remdups :: Ord a => [a] -> [a]
remdups = map head . group . sort

prop_rd_idempot :: Ord a => [a] -> Bool
prop_rd_idempot xs = remdups xs == (remdups . remdups) xs
