module Map.Creation
where

import Map.Types
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
addLakes m s = undefined
