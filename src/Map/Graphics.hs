{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Map.Graphics

(
mapVertexArrayDescriptor,
fgColorIndex,
fgNormalIndex,
fgVertexIndex,
mapStride,
getMapBufferObject
)
where

import Data.Array.IArray
import Prelude as P

--import Graphics.Rendering.OpenGL.GL
import           Graphics.Rendering.OpenGL.GL.BufferObjects
import           Graphics.Rendering.OpenGL.GL.ObjectName
import           Graphics.Rendering.OpenGL.GL.StateVar
import           Graphics.Rendering.OpenGL.GL.VertexArrays
import           Graphics.Rendering.OpenGL.GL.VertexSpec
import           Graphics.Rendering.OpenGL.Raw.Core31

import Foreign.Marshal.Array (withArray)
import Foreign.Storable      (sizeOf)
import Foreign.Ptr           (Ptr, nullPtr, plusPtr)
import Render.Misc           (checkError)
import Linear
import Control.Arrow         ((***))

import Map.Types

type Height = Double

type MapEntry = (
                Height,
                TileType
                )
type GraphicsMap = Array (Int, Int) MapEntry

-- converts from classical x/z to striped version of a map
convertToStripeMap :: PlayMap -> PlayMap
convertToStripeMap mp = array (stripify l, stripify u) (map (stripify *** strp) (assocs mp))
  where
    (l,u) = bounds mp

stripify :: (Int,Int) -> (Int,Int)
stripify (x,z) = (if even z then 2*x else 2*x+1, z `div` 2)

strp :: Node -> Node
strp (Node i (x,z,y) tt bi pli p ri si) = Node (stripify i) (x,z,y) tt bi pli p ri si

-- extract graphics information from Playmap
convertToGraphicsMap :: PlayMap -> GraphicsMap
convertToGraphicsMap mp = array (bounds mp) [(i, graphicsyfy (mp ! i))| i <- indices mp]
    where
      graphicsyfy :: Node -> MapEntry
      graphicsyfy (Node _ (_,_,y) t _ _ _ _ _ ) = (y, t)

lineHeight :: GLfloat
lineHeight = 0.8660254

-- Number of GLfloats per Stride
numComponents :: Int
numComponents = 10

mapStride :: Stride
mapStride = fromIntegral (sizeOf (0.0 :: GLfloat) * numComponents)

bufferObjectPtr :: Integral a => a -> Ptr GLfloat
bufferObjectPtr = plusPtr (nullPtr :: Ptr GLfloat) . fromIntegral

mapVertexArrayDescriptor :: NumComponents -> NumComponents -> VertexArrayDescriptor GLfloat
mapVertexArrayDescriptor count' offset =
   VertexArrayDescriptor count' Float mapStride (bufferObjectPtr (fromIntegral offset * sizeOf (0 :: GLfloat)) ) --(fromIntegral numComponents * offset))

fgColorIndex :: (IntegerHandling, VertexArrayDescriptor GLfloat)
fgColorIndex = (ToFloat, mapVertexArrayDescriptor 4 0)  --color first

fgNormalIndex :: (IntegerHandling, VertexArrayDescriptor GLfloat)
fgNormalIndex = (ToFloat, mapVertexArrayDescriptor 3 4) --normal after color

fgVertexIndex :: (IntegerHandling, VertexArrayDescriptor GLfloat)
fgVertexIndex = (ToFloat, mapVertexArrayDescriptor 3 7) --vertex after normal

getMapBufferObject :: PlayMap -> IO (BufferObject, NumArrayIndices)
getMapBufferObject eMap = do
        myMap'  <- return $ convertToGraphicsMap $ convertToStripeMap eMap
        ! myMap <- return $ generateTriangles myMap'
        len <- return $ fromIntegral $ P.length myMap `div` numComponents
        putStrLn $ P.unwords ["num verts in map:",show len]
        bo <- genObjectName                     -- create a new buffer
        bindBuffer ArrayBuffer $= Just bo       -- bind buffer
        withArray myMap $ \buffer ->
                bufferData ArrayBuffer $= (fromIntegral $ sizeOf (0 :: GLfloat) * P.length myMap,
                                           buffer,
                                           StaticDraw)
        checkError "initBuffer"
        return (bo,len)

--generateTriangles :: PlayMap -> [GLfloat]
generateTriangles :: GraphicsMap -> [GLfloat]
generateTriangles map' =
                let ((xl,yl),(xh,yh)) = bounds map' in
                P.concat [P.concat $ P.map (generateFirstTriLine map' y) [xl .. xh - 2]
                          ++ P.map (generateSecondTriLine map' (y == yh) y) [xl .. xh - 2]
                         | y <- [yl..yh]]

generateFirstTriLine :: GraphicsMap -> Int -> Int -> [GLfloat]
generateFirstTriLine map' y x =
                P.concat $
                   if even x then
                     [  lookupVertex map' x y,
                        lookupVertex map' (x + 1) y,
                        lookupVertex map' (x + 2) y
                     ]
                   else
                     [  lookupVertex map' x y,
                        lookupVertex map' (x + 2) y,
                        lookupVertex map' (x + 1) y
                     ]

generateSecondTriLine :: GraphicsMap -> Bool -> Int -> Int ->  [GLfloat]
generateSecondTriLine map' False y x  =
                P.concat $
                   if even x then
                     [  lookupVertex map' x (y + 1),
                        lookupVertex map' (x + 2) (y + 1),
                        lookupVertex map' (x + 1) y
                     ]
                   else
                     [  lookupVertex map' x y,
                        lookupVertex map' (x + 1) (y + 1),
                        lookupVertex map' (x + 2) y
                     ]
generateSecondTriLine _ True _ _  = []


lookupVertex :: GraphicsMap -> Int -> Int -> [GLfloat]
lookupVertex map' x y =
                let
                        (cr, cg, cb)  = colorLookup map' (x,y)
                        (V3 vx vy vz) = coordLookup (x,y) $ heightLookup map' (x,y)
                        (V3 nx ny nz) = normalLookup map' x y
                        --TODO: calculate normals correctly!
                in
                [
                        cr, cg, cb, 1.0,        -- RGBA Color
                        nx, ny, nz,             -- 3 Normal
                        vx, vy, vz              -- 3 Vertex
                ]

normalLookup :: GraphicsMap -> Int -> Int -> V3 GLfloat
normalLookup map' x y = normalize $ normN + normNE + normSE + normS + normSW + normNW
                    where
                      --Face Normals
                      normN  = cross (vNE-vC) (vNW-vC)
                      normNE = cross (vE -vC) (vNE-vC)
                      normSE = cross (vSE-vC) (vE -vC)
                      normS  = cross (vSW-vC) (vSE-vC)
                      normSW = cross (vW -vC) (vSW-vC)
                      normNW = cross (vNW-vC) (vW -vC)
                      --Vertex Normals
                      vC     = coordLookup (x,y) $ heightLookup map' (x,y)
                      --TODO: kill guards with eo
                      vNW
                        | even x    = coordLookup (x-1,y-1) $ heightLookup map' (x-1,y-1)
                        | otherwise = coordLookup (x-1,y  ) $ heightLookup map' (x-1,y  )
                      vNE
                        | even x    = coordLookup (x+1,y-1) $ heightLookup map' (x+1,y-1)
                        | otherwise = coordLookup (x+1,y  ) $ heightLookup map' (x+1,y  )
                      vE
                        | even x    = coordLookup (x+2,y  ) $ heightLookup map' (x+2,y  )
                        | otherwise = coordLookup (x+2,y  ) $ heightLookup map' (x+2,y  )
                      vSE
                        | even x    = coordLookup (x+1,y  ) $ heightLookup map' (x+1,y  )
                        | otherwise = coordLookup (x+1,y+1) $ heightLookup map' (x+1,y+1)
                      vSW
                        | even x    = coordLookup (x-1,y  ) $ heightLookup map' (x-1,y  )
                        | otherwise = coordLookup (x-1,y+1) $ heightLookup map' (x-1,y+1)
                      vW
                        | even x    = coordLookup (x-2,y  ) $ heightLookup map' (x-2,y  )
                        | otherwise = coordLookup (x-2,y  ) $ heightLookup map' (x-2,y  )
                      -- eo = if even x then 1 else -1

heightLookup :: GraphicsMap -> (Int,Int) -> GLfloat
heightLookup hs t = if inRange (bounds hs) t then fromRational $ toRational h else 0.0
                where
                        (h,_) = hs ! t

colorLookup :: GraphicsMap -> (Int,Int) -> (GLfloat, GLfloat, GLfloat)
colorLookup hs t = if inRange (bounds hs) t then c else (0.0, 0.0, 0.0)
                where
                        (_,tp) = hs ! t
                        c = case tp of
                                Ocean           -> (0.50, 0.50, 1.00)
                                Lake            -> (0.40, 0.87 ,1.00)
                                Beach           -> (0.90, 0.85, 0.70)
                                Desert          -> (1.00, 0.87, 0.39)
                                Grass           -> (0.30, 0.90, 0.10)
                                Mountain        -> (0.80, 0.80, 0.80)
                                Hill            -> (0.50, 0.50, 0.50)

coordLookup :: (Int,Int) -> GLfloat -> V3 GLfloat
coordLookup (x,z) y =
                if even x then
                        V3 (fromIntegral $ x `div` 2) y (fromIntegral (2 * z) * lineHeight)
                else
                        V3 (fromIntegral (x `div` 2) + 0.5) y (fromIntegral (2 * z + 1) * lineHeight)
