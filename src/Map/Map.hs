{-# LANGUAGE OverloadedStrings #-}
module Map.Map 

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


data TileType =
        Grass
        | Sand
        | Water
        | Mountain
        deriving (Show, Eq)

type MapEntry = (
                Float, -- ^ Height
                TileType
                )

type PlayMap = Array (Int, Int) MapEntry

lineHeight :: GLfloat
lineHeight = 0.8660254

numComponents :: Int
numComponents = 3

mapStride :: Stride
mapStride = fromIntegral (sizeOf (0.0 :: GLfloat)) * fromIntegral numComponents

bufferObjectPtr :: Integral a => a -> Ptr b
bufferObjectPtr = plusPtr (nullPtr :: Ptr GLchar) . fromIntegral

mapVertexArrayDescriptor :: NumComponents -> NumComponents -> VertexArrayDescriptor a
mapVertexArrayDescriptor count' offset =
   VertexArrayDescriptor count' Float mapStride (bufferObjectPtr (fromIntegral numComponents * offset))

fgColorIndex :: (IntegerHandling, VertexArrayDescriptor a)
fgColorIndex = (ToFloat, mapVertexArrayDescriptor 4 0)  --color first

fgNormalIndex :: (IntegerHandling, VertexArrayDescriptor a)
fgNormalIndex = (ToFloat, mapVertexArrayDescriptor 3 4) --normal after color

fgVertexIndex :: (IntegerHandling, VertexArrayDescriptor a)
fgVertexIndex = (ToFloat, mapVertexArrayDescriptor 3 7) --vertex after normal

getMapBufferObject :: IO (BufferObject, NumArrayIndices)
getMapBufferObject = do
        map' <- testmap
        map' <- return $ generateCube --generateTriangles map'
        putStrLn $ P.unlines $ P.map show (prettyMap map')
        len <- return $ fromIntegral $ P.length map' `div` numComponents
        putStrLn $ P.unwords ["num verts",show len]
        bo <- genObjectName                     -- create a new buffer
        bindBuffer ArrayBuffer $= Just bo       -- bind buffer
        withArray map' $ \buffer ->
                bufferData ArrayBuffer $= (fromIntegral (P.length map' * sizeOf(P.head map')), buffer, StaticDraw)
        checkError "initBuffer"
        return (bo,len)

prettyMap :: [GLfloat] -> [(GLfloat,GLfloat,GLfloat,GLfloat,GLfloat,GLfloat,GLfloat,GLfloat,GLfloat,GLfloat)]
prettyMap (a:b:c:d:x:y:z:u:v:w:ms) = (a,b,c,d,x,y,z,u,v,w):(prettyMap ms)
prettyMap _ = []

generateCube :: [GLfloat]
generateCube = [  -- lower plane
                  -0.3,-0.3,-0.3,
                  0.3,-0.3,0.3,
                  0.3,-0.3,-0.3,
                  -0.3,-0.3,-0.3,
                  -0.3,-0.3,0.3,
                  0.3,-0.3,0.3,
                  -- upper plane
                  -0.3,0.3,-0.3,
                  0.3,0.3,0.3,
                  0.3,0.3,-0.3,
                  -0.3,0.3,-0.3,
                  -0.3,0.3,0.3,
                  0.3,0.3,0.3,
                   -- left plane
                  -0.3,-0.3,-0.3,
                  -0.3,0.3,0.3,
                  -0.3,-0.3,0.3,
                  -0.3,-0.3,-0.3,
                  -0.3,0.3,0.3,
                  -0.3,0.3,-0.3,
                   -- right plane
                  0.3,-0.3,-0.3,
                  0.3,0.3,0.3,
                  0.3,-0.3,0.3,
                  0.3,-0.3,-0.3,
                  0.3,0.3,0.3,
                  0.3,0.3,-0.3,
                   -- front plane
                  -0.3,-0.3,-0.3,
                  0.3,0.3,-0.3,
                  0.3,-0.3,-0.3,
                  -0.3,-0.3,-0.3,
                  0.3,0.3,-0.3,
                  -0.3,0.3,-0.3,
                   -- back plane
                  -0.3,-0.3,0.3,
                  0.3,0.3,0.3,
                  0.3,-0.3,0.3,
                  -0.3,-0.3,0.3,
                  0.3,0.3,0.3,
                  -0.3,0.3,0.3
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
                        (cr, cg, cb) = colorLookup map' (x,y)
                        (vx, vy, vz) = coordLookup (x,y) $ heightLookup map' (x,y)
                        (nx, ny, nz) = (0.0, 1.0, 0.0) :: (GLfloat, GLfloat, GLfloat)
                        --TODO: calculate normals correctly!
                in
                [
                        cr, cg, cb, 1.0,        -- RGBA Color
                        nx, ny, nz,             -- 3 Normal
                        vx, vy, vz              -- 3 Vertex
                ]

heightLookup :: PlayMap -> (Int,Int) -> GLfloat
heightLookup hs t = if inRange (bounds hs) t then fromRational $ toRational h else 0.0
                where 
                        (h,_) = hs ! t

colorLookup :: PlayMap -> (Int,Int) -> (GLfloat, GLfloat, GLfloat)
colorLookup hs t = if inRange (bounds hs) t then c else (0.0, 0.0, 0.0)
                where 
                        (_,tp) = hs ! t
                        c = case tp of
                                Water           -> (0.5, 0.5, 1)
                                Sand            -> (0.9, 0.85, 0.7)
                                Grass           -> (0.3, 0.9, 0.1)
                                Mountain        -> (0.5, 0.5, 0.5)

coordLookup :: (Int,Int) -> GLfloat -> (GLfloat, GLfloat, GLfloat)
coordLookup (x,z) y = 
                if even x then
                        (fromIntegral $ x `div` 2, y, fromIntegral (2 * z) * lineHeight)
                else
                        (fromIntegral (x `div` 2) / 2.0, y, fromIntegral (2 * z + 1) * lineHeight)


-- if writing in ASCII-Format transpose so i,j -> y,x
-- row-minor -> row-major
testMapTemplate :: [Text]
testMapTemplate = T.transpose [
                "~~~~~~~~~~~~~~~~~~~~",
                "~~SSSSSSSSSSSSSS~~~~",
                "~SSGGGGGGGSGSGGS~~~~",
                "~SSGGGGGGMSGSGMS~~~~",
                "~SGGGGGGMMMGGGS~~~S~",
                "~SGGGMGMMMMMGGS~~~SS",
                "~GGGGGGGGGGGGGGS~~~~",
                "~SGGGGGGGGGGGGGS~~~~",
                "~~SSSSGGGSSSSS~~~~~~",
                "~~~~~SGGGGS~~~~~~~~~",
                "~~~~SSGGGGSS~~~~~~~~",
                "~~SSSGGGGGGSSSSS~~~~",
                "~SSGSGSGGGSGSGGS~~~~",
                "~SSGSGSGGMSGSGMS~~~~",
                "~SGGMMMMGGGGGGS~~~~~",
                "~SGMMMMMGGGGSSS~~~~~",
                "~GGMMMMMGGGSSSSS~~~~",
                "~SGGGGGGGSSSSSSS~~~~",
                "~~SSSSSSSSSSSS~~~~~~",
                "~~~~~~~~~~~~~~~~~~~~"
                ]

testMapTemplate2 :: [Text]
testMapTemplate2 = T.transpose [
                "~~~~~~"
                ]

testmap :: IO PlayMap
testmap = do
                g <- getStdGen
                rawMap <- return $ parseTemplate (randoms g) (T.concat testMapTemplate2)
                return $ listArray ((0,0),(5,0)) rawMap


parseTemplate :: [Int] -> Text -> [MapEntry]
parseTemplate (r:rs) t = 
        (case T.head t of
                '~' -> (0, Water)
                'S' -> (0, Sand)
                'G' -> (fromIntegral (r `mod` 3)/2.0,Grass)
                'M' -> (fromIntegral (r `mod` 3 + 2)/2.0, Mountain)
                _ -> error "invalid template format for map"
         ):parseTemplate rs (T.tail t)
parseTemplate [] _ = error "out of randoms.."
