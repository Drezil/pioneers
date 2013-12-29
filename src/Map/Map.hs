{-# LANGUAGE OverloadedStrings #-}
module Map.Map 

where

import System.Random
import Data.Array.IArray
import Data.Text as T 
import Prelude as P

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

-- if writing in ASCII-Format transpose so i,j -> y,x
-- row-minor -> row-major
testMapTemplate :: [Text]
testMapTemplate = T.transpose [
                "~~~~~~~~~~",
                "~~SSSSSS~~",
                "~SSGGGGS~~",
                "~SSGGMMS~~",
                "~SGGMMS~~~",
                "~SGMMMS~~~",
                "~GGGGGGS~~",
                "~SGGGGGS~~",
                "~~SSSS~~~~",
                "~~~~~~~~~~"
                ]

testmap :: IO PlayMap
testmap = do
                g <- getStdGen
                rawMap <- return $ parseTemplate (randoms g) (T.concat testMapTemplate)
                return $ listArray ((0,0),(9,9)) rawMap


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
