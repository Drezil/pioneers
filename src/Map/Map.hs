module Map.Map 

where

import System.Random
import Data.Array.IArray

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

testMapTemplate :: [[String]]
testMapTemplate = [
                ["~~~~~~~~~~"],
                ["~~SSSSSS~~"],
                ["~SSGGGGS~~"],
                ["~SSGGMMS~~"],
                ["~SGGMMS~~~"],
                ["~SGMMMS~~~"],
                ["~GGGGGGS~~"],
                ["~SGGGGGS~~"],
                ["~~SSSS~~~~"],
                ["~~~~~~~~~~"]
                ]

testmap :: IO PlayMap
testmap = do
                g <- getStdGen
                rawMap <- return $ map (parseTemplate (randoms g)) (concat $ concat testMapTemplate)
                return $ listArray ((0,0),(9,9)) rawMap


parseTemplate :: [Int] -> Char -> MapEntry
parseTemplate (r:_) t = 
        case t of
                '~' -> (0, Water)
                'S' -> (0, Sand)
                'G' -> ((fromIntegral $ r `mod` 3)/3,Grass)
                'M' -> ((fromIntegral $ r `mod` 3 + 2)/3, Mountain)
                _ -> error "invalid template format for map"
parseTemplate [] _ = error "out of randoms..."
