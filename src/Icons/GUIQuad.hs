module Icons.GUIQuad (GUIQuad(..), Icon(..), marshalIcon) where

import Data.Word (Word8)

type Coord = (Float, Float)
type ZIndex = Float

data GUIQuad = GUIQuad Coord Coord ZIndex Icon

data Icon = 
          Woodcutter
        | Stonemason
--
        | CloseButton
        | NextButton
        | PreviousButton

numIcons :: Int
numIcons = 32

sizeIcon :: Float
sizeIcon = 1.0/(fromIntegral numIcons)

iconToTex :: Icon -> Coord
iconToTex i =
    (x,y)
    where
        x = (fromIntegral (num `mod` numIcons)) * sizeIcon
        y = (fromIntegral (num `div` numIcons)) * sizeIcon
        num = fromIntegral.marshalIcon $ i


marshalIcon :: Icon -> Word8
marshalIcon a = case a of
    Woodcutter     -> 0
    Stonemason     -> 1
--
    CloseButton    -> 32
    NextButton     -> 33
    PreviousButton -> 34
