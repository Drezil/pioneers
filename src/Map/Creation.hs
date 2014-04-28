module Map.Creation
where

import Map.Types
-- import Map.Map unused (for now)

import Data.Array

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
aplAll fs m = foldl (\ n f -> f n) m fs

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
