{-# LANGUAGE ExistentialQuantification #-}

module UI.Callbacks where

import Control.Monad.Trans (liftIO)
import Types
import UI.UIBaseData
import UI.UIClasses
import UI.UIOperations

import qualified Graphics.Rendering.OpenGL.GL         as GL
import           Control.Lens                         ((^.), (.~), (%~))
import           Render.Misc                          (genColorData)
import           Foreign.Marshal.Array (pokeArray)
import           Foreign.Marshal.Alloc (allocaBytes)
import           Control.Monad.RWS.Strict             (get, liftIO, modify)


data Pixel = Pixel Int Int

getGUI :: [GUIAny]
getGUI = [ toGUIAny $ GUIContainer 0 0 120 80 [] 1
         , toGUIAny $ GUIPanel $ GUIContainer 0 0 0 0
             [toGUIAny $ GUIContainer  0 80 100 200 [] 4
             ,toGUIAny $GUIButton 50 400 200 175 2 defaultUIState testMessage
             ] 3
         ]

testMessage :: (Show w) => w -> ScreenUnit -> ScreenUnit -> IO w
testMessage w x y = do
    putStrLn ("\tclick on " ++ show x ++ "," ++ show y)
    return w

-- | Handler for UI-Inputs.
--   Indicates a primary click on something (e.g. left-click, touch on Touchpad, fire on Gamepad, ...
clickHandler :: Pixel -> Pioneers ()
clickHandler (Pixel x y) = case concatMap (isInside x y) getGUI of
    [] -> liftIO $ putStrLn $ unwords ["button press on (",show x,",",show y,")"]
    hit -> liftIO $ do
        _ <- sequence $ map (\w ->
            case w of
                 (GUIAnyB b h) -> do
                      putStrLn $ "hitting " ++ getShorthand w ++ ": " ++ show (getBoundary w) ++ " " ++ show (getPriority w)
                          ++ " at ["++show x++","++show y++"]"
                      (b', h') <- onMousePressed x y b h
                      _ <- onMouseReleased x y b' h'
                      return ()
                 _ -> putStrLn $ "hitting " ++ getShorthand w ++ ": " ++ show (getBoundary w) ++ " " ++ show (getPriority w)
                          ++ " at ["++show x++","++show y++"]"
            ) hit
        return ()


-- | Handler for UI-Inputs.
--   Indicates an alternate click on something (e.g. right-click, touch&hold on Touchpad, ...
alternateClickHandler :: Pixel -> Pioneers ()
alternateClickHandler (Pixel x y) = liftIO $ putStrLn $ unwords ["alternate press on (",show x,",",show y,")"]


-- | informs the GUI to prepare a blitting of state ^. gl.glHud.hudTexture
--
--TODO: should be done asynchronously at one point.
--        -> can't. if 2 Threads bind Textures its not sure
--           on which one the GPU will work.
--           "GL.textureBinding GL.Texture2D" is a State set
--           to the texture all following works on.
--
--           https://www.opengl.org/wiki/GLAPI/glTexSubImage2D for copy
prepareGUI :: Pioneers ()
prepareGUI = do
                state <- get
                let tex = (state ^. gl.glHud.hudTexture)
                liftIO $ do
                    -- bind texture - all later calls work on this one.
                    GL.textureBinding GL.Texture2D GL.$= Just tex
                    mapM_ (copyGUI tex) getGUI
                modify $ ui.uiHasChanged .~ False

--TODO: Perform border-checking ... is xoff + width and yoff+height inside the screen-coordinates..
copyGUI :: GL.TextureObject -> GUIAny -> IO ()
copyGUI tex widget = do
                        let (xoff, yoff, width, height) = getBoundary widget
                            int = fromInteger.toInteger --conversion between Int8, GLInt, Int, ...
                            --temporary color here. lateron better some getData-function to
                            --get a list of pixel-data or a texture.
                            color = case widget of
                                (GUIAnyC _)   -> [255,0,0,128]
                                (GUIAnyB _ _) -> [255,255,0,255]
                                (GUIAnyP _)   -> [128,128,128,255]
                                _             -> [255,0,255,255]
                        allocaBytes (width*height*4) $ \ptr -> do
                                --copy data into C-Array
                                pokeArray ptr (genColorData (width*height) color)
                                GL.texSubImage2D
                                        GL.Texture2D
                                        0
                                        (GL.TexturePosition2D (int xoff) (int yoff))
                                        (GL.TextureSize2D (int width) (int height))
                                        (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
                        mapM_ (copyGUI tex) (getChildren widget)
copyGUI _ _ = return ()

--TODO: Add scroll-Handler, return (Pioneers Bool) to indicate event-bubbling etc.
--TODO: Maybe queues are better?
