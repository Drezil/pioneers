{-# LANGUAGE RankNTypes #-}
module Render.Types (createFlatCam, createSphereCam, Camera, GLCamera(..)) where

import Linear
import Foreign.C (CFloat)
import Render.Misc (lookAt)
import Map.Map (giveMapHeight)
import Map.Types (PlayMap)
import GHC.Float
import qualified Debug.Trace as D

type Distance = Double
type Pitch = Double
type Yaw = Double

class GLCamera a where
  getCam :: a -> Distance -> Pitch -> Yaw -> M44 CFloat
  moveBy :: a -> (Position -> Position) -> PlayMap -> a
  move   :: a -> Position -> PlayMap -> a

type Position = (Double, Double)

type Radius = Double

type Height = Double

data Camera = Flat Position Height
            | Sphere Position Radius

-- | create a Flatcam-Camera starting at given x/z-Coordinates
createFlatCam :: Double -> Double -> PlayMap -> Camera
createFlatCam x z map' = Flat (x,z) (float2Double $ giveMapHeight map' (double2Float x,double2Float z))

-- | create a Flatcam-Camera starting at given pitch/azimuth/radius
createSphereCam :: Double -> Double -> Double -> Camera
createSphereCam p a = Sphere (p,a)


instance GLCamera Camera where
  getCam (Flat (x',z') y') dist' xa' ya' =
        lookAt (cpos ^+^ at') at' up
                     where
                        at'   = V3 x y z
                        cpos  = crot !* (V3 0 0 (-dist))
                        crot  = (
                                (fromQuaternion $ axisAngle upmap (xa::CFloat))
                                !*!
                                (fromQuaternion $ axisAngle (V3 0 1 0) (ya::CFloat))
                                ) ::M33 CFloat
                        upmap = ((fromQuaternion $ axisAngle (V3 0 1 0) (ya::CFloat)) :: M33 CFloat)
                                !* (V3 1 0 0)
                        x     = realToFrac x'
                        y     = realToFrac y'
                        z     = realToFrac z'
                        dist  = realToFrac dist'
                        xa    = realToFrac xa'
                        ya    = realToFrac ya'
                        up    = V3 0 1 0
  getCam (Sphere (inc',az') r') dist' xa' ya' = --inclination (pitch), azimuth (yaw)
        lookAt (cpos ^+^ at') at' up
                     where
                        at'   = sphereToCart r inc az
                        cpos  = crot !* (V3 0 0 (-dist))
                        crot  = (
                                (fromQuaternion $ axisAngle upmap (xa::CFloat))
                                !*!
                                (fromQuaternion $ axisAngle (V3 0 1 0) (ya::CFloat))
                                ) ::M33 CFloat
                        upmap = ((fromQuaternion $ axisAngle (V3 0 1 0) (ya::CFloat)) :: M33 CFloat)
                                !* (V3 1 0 0)
                        up    = (sphereToCart (r+1) inc az) ^-^ at'
                        r     = realToFrac r'
                        inc   = realToFrac inc'
                        az    = realToFrac az'
                        dist  = realToFrac dist'
                        xa    = realToFrac xa'
                        ya    = realToFrac ya'
  moveBy (Sphere (inc, az) r) f map = undefined
  moveBy (Flat (x', z') y) f map = Flat (x,z) (float2Double y)
				where
					(x,z) = f (x', z')
					y = giveMapHeight map (fc x,fc z)
					fc = double2Float
  move c (x', z') map = moveBy c (\(x,z) -> (x+x',z+z')) map

sphereToCart :: (Floating a) => a -> a -> a -> V3 a
sphereToCart r inc az = V3
                                   (r * (sin inc) * (cos az))
                                   (r * (sin inc) * (sin az))
                                   (r * (cos inc))
