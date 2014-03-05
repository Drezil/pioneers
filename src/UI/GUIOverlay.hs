module UI.GUIOverlay where

import Data.Int
import Graphics.UI.SDL.Surface
import Graphics.UI.SDL.Color
import Graphics.UI.SDL.Rect
import Graphics.UI.SDL.Types

--createRGBSurface :: Int32 -> Int32 -> Int32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO Surface
--                    width    height   depth    rFilter   gFilter   bFilter   aFilter

updateGUI :: Int32 -> Int32 -> IO Surface
updateGUI width height = do
      overlay <- createRGBSurface width height 32 0xFF000000 0x00FF0000 0x0000FF00 0x000000FF
      fillRect overlay (Rect 10 10 400 300) (Color 255 0 128 255)
      return overlay
      
      
--createTextureFromSurface :: Renderer -> Surface -> IO Texture
--createSoftwareRenderer :: Surface -> IO Renderer