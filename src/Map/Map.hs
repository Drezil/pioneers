{-# LANGUAGE OverloadedStrings #-}
module Map.Map 

where

import System.Random
import Data.Array.IArray
import Data.Text as T 
import Prelude as P
import Graphics.Rendering.OpenGL.Raw.Core31.Types (GLfloat)


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

-- | getMap returns the map as List of Vertices (rendered as triangles).
--   This promises to hold True for length v == length c == length n in
--   getMap -> (v,c,n) with length v `mod` 3 == 0.
--
--   v are Vertices, c are Colors and n are Normals. 
getMap :: IO ([GLfloat], [GLfloat], [GLfloat])
getMap = do
                map' <- testmap
                return $ unzip3 $ generateTriangles map'


generateTriangles :: PlayMap -> [(GLfloat, GLfloat, GLfloat)] 
generateTriangles map' =
                let ((xl,yl),(xh,yh)) = bounds map' in
                P.concat [P.concat $ P.map (generateFirstTriLine map' y) [xl .. xh - 2] 
                                  ++ P.map (generateSecondTriLine map' (y == yh) y) [xl .. xh - 2]
                         | y <- [yl..yh]]

generateFirstTriLine :: PlayMap -> Int -> Int -> [(GLfloat, GLfloat, GLfloat)]
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

generateSecondTriLine :: PlayMap -> Bool -> Int -> Int ->  [(GLfloat, GLfloat, GLfloat)]
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


lookupVertex :: PlayMap -> Int -> Int -> [(GLfloat, GLfloat, GLfloat)]
lookupVertex map' x y = 
                let 
                        (cr, cg, cb) = colorLookup map' (x,y)
                        (vx, vy, vz) = coordLookup (x,y) $ heightLookup map' (x,y)
                        (nx, ny, nz) = (0.0, 1.0, 0.0) :: (GLfloat, GLfloat, GLfloat)
                in
                [
                        (vx, cr, nx),
                        (vy, cg, ny),
                        (vz, cb, nz)
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
coordLookup (x,y) h = 
                if even x then
                        (fromIntegral $ x `div` 2, fromIntegral (2 * y) * lineHeight, h)
                else
                        (fromIntegral (x `div` 2) / 2.0, fromIntegral (2 * y + 1) * lineHeight, h)


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

testmap :: IO PlayMap
testmap = do
                g <- getStdGen
                rawMap <- return $ parseTemplate (randoms g) (T.concat testMapTemplate)
                return $ listArray ((0,0),(19,19)) rawMap


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
