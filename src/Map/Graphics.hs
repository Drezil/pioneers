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

import System.Random
import Data.Array.IArray
import Data.Text as T 
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

import Map.Types
import Map.StaticMaps

type MapEntry = (
                Float, -- ^ Height
                TileType
                )

type GraphicsMap = Array (Int, Int) MapEntry

-- extract graphics information from Playmap
convertToGraphicsMap :: PlayMap -> GraphicsMap
convertToGraphicsMap map = array (bounds map) [(i, graphicsyfy (map!i))| i <- indices map]
    where
      graphicsyfy :: Node -> MapEntry
      graphicsyfy (Minimal _               ) = (0, Grass)
      graphicsyfy (Full    _ y t _ _ _ _ _ ) = (y, t)

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
   VertexArrayDescriptor count' Float mapStride (bufferObjectPtr ((fromIntegral offset)*sizeOf (0 :: GLfloat)) ) --(fromIntegral numComponents * offset))

fgColorIndex :: (IntegerHandling, VertexArrayDescriptor GLfloat)
fgColorIndex = (ToFloat, mapVertexArrayDescriptor 4 0)  --color first

fgNormalIndex :: (IntegerHandling, VertexArrayDescriptor GLfloat)
fgNormalIndex = (ToFloat, mapVertexArrayDescriptor 3 4) --normal after color

fgVertexIndex :: (IntegerHandling, VertexArrayDescriptor GLfloat)
fgVertexIndex = (ToFloat, mapVertexArrayDescriptor 3 7) --vertex after normal

getMapBufferObject :: IO (BufferObject, NumArrayIndices)
getMapBufferObject = do
        map'   <- return $ convertToGraphicsMap mapCenterMountain
        ! map' <- return $ generateTriangles map'
        len <- return $ fromIntegral $ P.length map' `div` numComponents
        putStrLn $ P.unwords ["num verts in map:",show len]
        bo <- genObjectName                     -- create a new buffer
        bindBuffer ArrayBuffer $= Just bo       -- bind buffer
        withArray map' $ \buffer ->
                bufferData ArrayBuffer $= (fromIntegral $ sizeOf (0 :: GLfloat)*P.length map',
                                           buffer,
                                           StaticDraw)
        checkError "initBuffer"
        return (bo,len)

prettyMap :: [GLfloat] -> [(GLfloat,GLfloat,GLfloat,GLfloat,GLfloat,GLfloat,GLfloat,GLfloat,GLfloat,GLfloat)]
prettyMap (a:b:c:d:x:y:z:u:v:w:ms) = (a,b,c,d,x,y,z,u,v,w):(prettyMap ms)
prettyMap _ = []

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
                      eo = if even x then 1 else -1

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
                                Hill            -> (0.80, 0.80, 0.80)
                                Mountain        -> (0.50, 0.50, 0.50)

coordLookup :: (Int,Int) -> GLfloat -> V3 GLfloat
coordLookup (x,z) y =
                if even x then
                        V3 (fromIntegral $ x `div` 2) y (fromIntegral (2 * z) * lineHeight)
                else
                        V3 (fromIntegral (x `div` 2) + 0.5) y (fromIntegral (2 * z + 1) * lineHeight)


-- if writing in ASCII-Format transpose so i,j -> y,x
-- row-minor -> row-major
testMapTemplate :: [Text]
testMapTemplate = repText 2 $ T.transpose [
                "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
                "~~~SSSSSSSSSSSSSS~~~~~SSSSSSSSSSSSSS~~~~",
                "~~SSGGGGGGGSGSGGS~~~~SSGGGGGGGSGSGGS~~~~",
                "~~SSGGGGGGMSGSGGS~~~~SSGGGGGGMSGSGGS~~~~",
                "~~SGGGGGGMMMGGGS~~~S~SGGGGGGMMMGGGS~~~S~",
                "~~SGGGMGMMMMMGGS~~~SSSGGGMGMMMMMGGS~~~SS",
                "~~GGGGGGGGGGGGGGS~~~~GGGGGGGGGGGGGGS~~~~",
                "~~SGGGGGGGGGGGGGS~~~~SGGGGGGGGGGGGGS~~~~",
                "~~~SSSSGGGSSSSS~~~~~~~SSSSGGGSSSSS~~~~~~",
                "~~~~~~SGGGGS~~~~~~~~~~~~~SGGGGS~~~~~~~~~",
                "~~~~~SSGGGGSS~~~~~~~~~~~SSGGGGSS~~~~~~~~",
                "~~~SSSGGGGGGSSSSS~~~~~SSSGGGGGGSSSSS~~~~",
                "~~SSGSGSGGGSGSGGS~~~~SSGSGSGGGSGSGGS~~~~",
                "~~SSGSGSGGMSGSGMS~~~~SSGSGSGMMMMMSSS~~~~",
                "~~SGGMMMMGGGGGGS~~~~~SGGGGGGMMMMMSS~~~~~",
                "~~SGMMMMMGGGGSSS~~~~~SGGGGGGMMMMMSS~~~~~",
                "~~GGMMMMMGGGSSSSS~~~~GGGGGGGGGGSSSSS~~~~",
                "~~SGGGGGGGSSSSSSS~~~~SGGGGGGGSSSSSSS~~~~",
                "~~~SSSSSSSSSSSS~~~~~~~SSSSSSSSSSSS~~~~~~",
                "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                ]

repText :: Int -> [a] -> [a]
repText a (t:[]) = P.replicate a t
repText a ts     = P.concat $ P.map (repText' a) ts
                where
                        repText' :: Int -> a -> [a]
                        repText' a x = repText a [x]

testMapTemplate2 :: [Text]
testMapTemplate2 = T.transpose [
                "~~~~~~~~~~~~"
                ]

testmap :: IO GraphicsMap
testmap = do
                g <- getStdGen
                rawMap <- return $ parseTemplate (randoms g) (T.concat testMapTemplate)
                return $ listArray ((0,0),(79,19)) rawMap

testmap2 :: IO GraphicsMap
testmap2 = do
                g <- getStdGen
                rawMap <- return $ parseTemplate (randoms g) (T.concat testMapTemplate2)
                return $ listArray ((0,0),(9,0)) rawMap


parseTemplate :: [Int] -> Text -> [MapEntry]
parseTemplate (r:rs) t = 
        (case T.head t of
                '~' -> (0, Ocean)
                'S' -> (0, Beach)
                'G' -> (fromIntegral (r `mod` 10)/10.0,Grass)
                'M' -> (fromIntegral ((r `mod` 10) + 20)/10.0, Mountain)
                _ -> error "invalid template format for map"
         ):parseTemplate rs (T.tail t)
parseTemplate [] _ = error "out of randoms.."