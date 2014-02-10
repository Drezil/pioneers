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
import Foreign.Storable (sizeOf)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Render.Misc (checkError)
import Linear

import Map.Types

type MapEntry = (
                Float, -- ^ Height
                TileType
                )

type PlayMap = Array (Int, Int) MapEntry

lineHeight :: GLfloat
lineHeight = 0.8660254

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
        map' <- testmap
        ! map' <- return $ generateTriangles map'
        --putStrLn $ P.unlines $ P.map show (prettyMap map')
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

generateCube :: [GLfloat]
generateCube = [  -- lower plane
                  -3.0,-3.0,-3.0,
                  3.0,-3.0,3.0,
                  3.0,-3.0,-3.0,
                  -3.0,-3.0,-3.0,
                  -3.0,-3.0,3.0,
                  3.0,-3.0,3.0,
                  -- upper plane
                  -3.0,3.0,-3.0,
                  3.0,3.0,3.0,
                  3.0,3.0,-3.0,
                  -3.0,3.0,-3.0,
                  -3.0,3.0,3.0,
                  3.0,3.0,3.0,
                   -- left plane
                  -3.0,-3.0,-3.0,
                  -3.0,3.0,3.0,
                  -3.0,-3.0,3.0,
                  -3.0,-3.0,-3.0,
                  -3.0,3.0,3.0,
                  -3.0,3.0,-3.0,
                   -- right plane
                  3.0,-3.0,-3.0,
                  3.0,3.0,3.0,
                  3.0,-3.0,3.0,
                  3.0,-3.0,-3.0,
                  3.0,3.0,3.0,
                  3.0,3.0,-3.0,
                   -- front plane
                  -3.0,-3.0,-3.0,
                  3.0,3.0,-3.0,
                  3.0,-3.0,-3.0,
                  -3.0,-3.0,-3.0,
                  3.0,3.0,-3.0,
                  -3.0,3.0,-3.0,
                   -- back plane
                  -3.0,-3.0,3.0,
                  3.0,3.0,3.0,
                  3.0,-3.0,3.0,
                  -3.0,-3.0,3.0,
                  3.0,3.0,3.0,
                  -3.0,3.0,3.0
                  ]

generateTriangles :: PlayMap -> [GLfloat] 
generateTriangles map' =
                let ((xl,yl),(xh,yh)) = bounds map' in
                P.concat [P.concat $ P.map (generateFirstTriLine map' y) [xl .. xh - 2] 
                          ++ P.map (generateSecondTriLine map' (y == yh) y) [xl .. xh - 2]
                         | y <- [yl..yh]]

generateFirstTriLine :: PlayMap -> Int -> Int -> [GLfloat]
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

generateSecondTriLine :: PlayMap -> Bool -> Int -> Int ->  [GLfloat]
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


lookupVertex :: PlayMap -> Int -> Int -> [GLfloat]
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

normalLookup :: PlayMap -> Int -> Int -> V3 GLfloat
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

heightLookup :: PlayMap -> (Int,Int) -> GLfloat
heightLookup hs t = if inRange (bounds hs) t then fromRational $ toRational h else 0.0
                where 
                        (h,_) = hs ! t

colorLookup :: PlayMap -> (Int,Int) -> (GLfloat, GLfloat, GLfloat)
colorLookup hs t = if inRange (bounds hs) t then c else (0.0, 0.0, 0.0)
                where 
                        (_,tp) = hs ! t
                        c = case tp of
                                Ocean           -> (0.5, 0.5, 1)
                                Beach            -> (0.9, 0.85, 0.7)
                                Grass           -> (0.3, 0.9, 0.1)
                                Mountain        -> (0.5, 0.5, 0.5)

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

testmap :: IO PlayMap
testmap = do
                g <- getStdGen
                rawMap <- return $ parseTemplate (randoms g) (T.concat testMapTemplate)
                return $ listArray ((0,0),(79,19)) rawMap

testmap2 :: IO PlayMap
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
