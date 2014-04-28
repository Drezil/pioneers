module Map.Creation
where

import Map.Types
import Map.StaticMaps
-- import Map.Map unused (for now)

import Data.Array
import System.Random

-- preliminary
infix 5 ->-
(->-) :: (PlayMap -> PlayMap) -> (PlayMap -> PlayMap) -> PlayMap -> PlayMap
f ->- g = g . f

-- also preliminary
infix 5 -<-
(-<-) :: (PlayMap -> PlayMap) -> (PlayMap -> PlayMap) -> PlayMap -> PlayMap
f -<- g = f . g

exportedMap :: IO PlayMap
exportedMap = do mounts <- mnt
                 return $ aplAll mounts mapEmpty

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

aplAllM :: Monad m => [m a -> m a] -> m a -> m a
aplAllM fs x = foldl (\ n f -> f n) x fs

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


lake :: Int -> PlayMap -> PlayMap
lake = undefined

river :: Int -> PlayMap -> PlayMap
river = undefined

mnt :: IO [PlayMap -> PlayMap]
mnt = do g <- newStdGen
         let seeds = take 10 $ randoms g
         return $ map (gaussMountain) seeds

gaussMountain :: Int -> PlayMap -> PlayMap
gaussMountain seed mp = aplByPlace (liftUp c) (\(_,_) -> True) mp
  where
    g   = mkStdGen seed
    c   = let ((a,b), (x,y)) = bounds mp in (head (randomRs (a,x) g), (head (randomRs (b,y) g)))
    amp = head $ randomRs (5.0, 20.0) g
    sig = head $ randomRs (5.0, 25.0) g
    fi  = fromIntegral
    htt = heightToTerrain

    -- TODO: Fix Lambda to True with sensible function, maybe rework giveNeighbourhood in Map.Map
    liftUp :: (Int, Int) -> Node -> Node
    liftUp (gx,gz) (Full     (x,z) y _ b pl pa r s) = let y_neu = max y e
                                                      in  Full (x,z) y_neu (htt GrassIslandMap y_neu) b pl pa r s
      where e = gauss3Dgeneral amp (fi gx) (fi gz) sig sig (fi x) (fi z)
    liftUp (gx, gz) (Minimal (x,z)) = Full (x,z) e (htt GrassIslandMap e) BFlag NoPlayer NoPath Plain []
      where e = gauss3Dgeneral amp (fi gx) (fi gz) sig sig (fi x) (fi z)

-- | Makes sure the edges of the Map are mountain-free
makeIsland :: PlayMap -> PlayMap
makeIsland = undefined -- tomorrow....
