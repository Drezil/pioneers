module Map.Combinators where

import Map.Types
import Map.Creation

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

lake :: Int -> PlayMap -> PlayMap
lake = undefined

river :: Int -> PlayMap -> PlayMap
river = undefined

mnt :: IO [PlayMap -> PlayMap]
mnt = do g <- newStdGen
         let seeds = take 10 $ randoms g
         return $ map gaussMountain seeds

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
