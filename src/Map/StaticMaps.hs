module Map.StaticMaps
where

import Map.Types
import Data.Array
import Map.Creation (heightToTerrain)

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

-- entirely empty map, only uses the minimal constructor
mapEmpty :: PlayMap
mapEmpty = array ((0,0), (199,199)) [((a,b), (Minimal (a,b))) | a <- [0..199], b <- [0..199]]

-- TODO: Stripify
mapCenterMountain :: PlayMap
mapCenterMountain = array ((0,0),(199,199)) nodes
    where
      nodes    = water ++ beach ++ grass ++ hill ++ mountain
      water    = [((a,b), (Full (a,b) 0.0       Ocean    BNothing NoPlayer NoPath Plain [])) | a <- [0..199], b <- [0..199], (m2d (a,b)) > 95]
      beach    = [((a,b), (Full (a,b) (g2d a b) Beach    BNothing NoPlayer NoPath Plain [])) | a <- [0..199], b <- [0..199], (m2d (a,b)) <= 95, (m2d (a,b)) > 75]
      grass    = [((a,b), (Full (a,b) (g2d a b) Grass    BNothing NoPlayer NoPath Plain [])) | a <- [0..199], b <- [0..199], (m2d (a,b)) <= 75, (m2d (a,b)) > 25]
      hill     = [((a,b), (Full (a,b) (g2d a b) Hill     BNothing NoPlayer NoPath Plain [])) | a <- [0..199], b <- [0..199], (m2d (a,b)) <= 25, (m2d (a,b)) > 10]
      mountain = [((a,b), (Full (a,b) (g2d a b) Mountain BNothing NoPlayer NoPath Plain [])) | a <- [0..199], b <- [0..199], (m2d (a,b)) <= 10]

      g2d :: Int -> Int -> Float
      g2d x y = gauss3D (fromIntegral x) (fromIntegral y)

      m2d :: (Int,Int) -> Int
      m2d (x,y) = mnh2D (x,y) (100,100)

-- small helper for some hills. Should be replaced by multi-layer perlin-noise
-- TODO: Replace as given in comment.
_noisyMap :: (Floating q) => q -> q -> q
_noisyMap = \x y -> gauss3Dgeneral 15 100.0 100.0 15.0 15.0 x y
                +  gauss3Dgeneral 5  10.0 10.0 10.0 10.0 x y
                +  gauss3Dgeneral 5  150.0 120.0 10.0 10.0 x y
                +  gauss3Dgeneral 5  50.0 75.0 10.0 10.0 x y

-- generates a noisy map
-- TODO: add real noise to a simple pattern
mapNoise :: PlayMap
mapNoise = array ((0,0),(199,199)) nodes
    where
      nodes    = [((a,b), (Full
                            (a,b)
                            (height a b)
                            (heightToTerrain GrassIslandMap $ height a b)
                            BNothing
                            NoPlayer
                            NoPath
                            Plain
                            [])) | a <- [0..199], b <- [0..199]]
                 where
                    height a b = (_noisyMap (fromIntegral a) (fromIntegral b))
