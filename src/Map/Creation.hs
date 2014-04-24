module Map.Creation
where

import Map.Types
import Map.Map

import Data.Array
import System.Random

-- Orphan instance since this isn't where either Random nor Tuples are defined
instance (Random x, Random y) => Random (x, y) where
  randomR ((x1, y1), (x2, y2)) gen1 = let (a, gen2) = randomR (x1, x2) gen1
                                          (b, gen3) = randomR (y1, y2) gen2
                                      in ((a, b), gen3)

  random                       gen1 = let (a, gen2) = random gen1
                                          (b, gen3) = random gen2 in ((a,b), gen3)

-- | Generate a new Map of given Type and Size
--
--   TODO:
--   1. Should take Size -> Type -> Playmap
--   2. plug together helper-functions for that terraintype
newMap :: MapType -> (Int, Int) -> PlayMap
newMap = undefined

aplByPlace :: (Node -> Node) -> ((Int,Int) -> Bool) -> PlayMap -> PlayMap
aplByPlace f g mp = array (bounds mp) (map (\(ab,c) -> if g ab then (ab, f c) else (ab,c)) (assocs mp))

aplByNode :: (Node -> Node) -> (Node -> Bool) -> PlayMap -> PlayMap
aplByNode f g mp = array (bounds mp) (map (\(ab,c) -> (if g c then (ab, f c) else (ab,c))) (assocs mp)) 

aplAll :: [a -> a] -> a -> a
aplAll []     m = m
aplAll (f:fs) m = aplAll fs $ f m

-- general 3D-Gaussian
gauss3Dgeneral :: Floating q =>
                  q    -- ^ Amplitude
                  -> q -- ^ Origin on X-Achsis
                  -> q -- ^ Origin on Z-Achsis
                  -> q -- ^ Sigma on X
                  -> q -- ^ Sigma on Z
                  -> q -- ^ Coordinate in question on X
                  -> q -- ^ Coordinate in question on Z
                  -> q -- ^ elevation on coordinate in question
gauss3Dgeneral amp x0 z0 sX sZ x z = amp * exp(-(((x-x0)^(2 :: Integer)/(2 * sX^(2 :: Integer)))+((z-z0)^(2 :: Integer)/(2 * sZ^(2 :: Integer)))))

-- specialised 3D gaussian with an origin on 100/100, an amplitude of 15 and two sigmas of 15
gauss3D :: Floating q =>
           q     -- ^ X-Coordinate
           -> q  -- ^ Z-Coordinate
           -> q  -- ^ elevation on coordinate in quesion
gauss3D = gauss3Dgeneral 15 100.0 100.0 15.0 15.0

-- 2D Manhattan distance
mnh2D :: (Int,Int) -> (Int,Int) -> Int
mnh2D (a,b) (c,d) = abs (a-c) + abs (b-d)

-- | Basic Terrain-Generator. Will not implement "abnormal" Stuff for given Biome
--   (like Deserts on Grass-Islands or Grass on Deserts)
--
--   TODO: Implement Desert-Generator
heightToTerrain :: MapType -> YCoord -> TileType
heightToTerrain GrassIslandMap y
                | y < 0.1   = Ocean
                | y < 1     = Beach
                | y < 5     = Grass
                | y < 10    = Hill
                | otherwise = Mountain
heightToTerrain _ _ = undefined

type Seed = (XCoord, ZCoord)

-- | Add lakes on generated Map from (possible) Seeds noted before.
--
--   TODO: implement and erode terrain on the way down.
addLakes :: PlayMap -> [Seed] -> PlayMap
addLakes = undefined

gaussMountain :: Int -> PlayMap -> PlayMap
gaussMountain seed mp = aplByPlace (liftUp c) (\(_,_) -> True) mp
  where
    g   = mkStdGen seed
    c   = head $ randomRs (bounds mp) g
    fi  = fromIntegral
    htt = heightToTerrain

    -- TODO: Fix Lambda to True with sensible function, maybe rework giveNeighbourhood in Map.Map
    liftUp :: (Int, Int) -> Node -> Node
    liftUp (gx,gz) (Full     (x,z) y _ b pl pa r s) = let y_neu = max y e 
                                                      in  (Full (x,z) y_neu (htt GrassIslandMap y_neu) b pl pa r s)
      where e = gauss3Dgeneral 10.0 (fi gx) (fi gz) 5.0 5.0 (fi x) (fi z)
    liftUp (gx, gz) (Minimal (x,z)) = Full (x,z) e (htt GrassIslandMap e) BFlag NoPlayer NoPath Plain []
      where e = gauss3Dgeneral 10.0 (fi gx) (fi gz) 5.0 5.0 (fi x) (fi z)
