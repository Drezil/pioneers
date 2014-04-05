module Map.StaticMaps
where

import Map.Types
import Data.Array

gauss2Dgeneral :: Floating q => q -> q -> q -> q -> q -> q -> q -> q
gauss2Dgeneral amp x0 y0 sX sY x y = amp * exp (-(((x-x0)^2/(2 * sX^2))+((y-y0)^2/(2 * sY^2))))

gauss2D :: Floating q => q -> q -> q
gauss2D x y = gauss2Dgeneral 45 100.0 100.0 50.0 50.0 x y

-- 2D Manhattan distance
mnh2D :: (Int,Int) -> (Int,Int) -> Int
mnh2D (a,b) (c,d) = abs (a-c) + abs (b-d)

-- entirely empty map, only uses the minimal constructor
mapEmpty :: PlayMap
mapEmpty = array ((0,0), (200,200)) [((a,b), (Minimal (a,b))) | a <- [0..200], b <- [0..200]]

-- TODO: Stripify
mapCenterMountain :: PlayMap
mapCenterMountain = array ((0,0),(200,200)) nodes
    where
      nodes    = water ++ beach ++ grass ++ hill ++ mountain
      water    = [((a,b), (Full (a,b) 0.0       Ocean    BNothing NoPlayer NoPath Plain [])) | a <- [0..200], b <- [0..200], (m2d (a,b)) > 95] 
      beach    = [((a,b), (Full (a,b) (g2d a b) Beach    BNothing NoPlayer NoPath Plain [])) | a <- [0..200], b <- [0..200], (m2d (a,b)) <= 95, (m2d (a,b)) > 75]
      grass    = [((a,b), (Full (a,b) (g2d a b) Grass    BNothing NoPlayer NoPath Plain [])) | a <- [0..200], b <- [0..200], (m2d (a,b)) <= 75, (m2d (a,b)) > 25]
      hill     = [((a,b), (Full (a,b) (g2d a b) Hill     BNothing NoPlayer NoPath Plain [])) | a <- [0..200], b <- [0..200], (m2d (a,b)) <= 25, (m2d (a,b)) > 10]
      mountain = [((a,b), (Full (a,b) (g2d a b) Mountain BNothing NoPlayer NoPath Plain [])) | a <- [0..200], b <- [0..200], (m2d (a,b)) <= 10]
     
      g2d :: Int -> Int -> Float
      g2d x y = gauss2D (fromIntegral x) (fromIntegral y)

      m2d :: (Int,Int) -> Int
      m2d (x,y) = mnh2D (x,y) (100,100)

