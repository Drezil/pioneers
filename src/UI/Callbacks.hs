module UI.Callbacks where


import qualified Graphics.Rendering.OpenGL.GL         as GL
import           Control.Lens                         ((^.), (.~), (%~))
import           Control.Monad                        (liftM, when, unless)
import           Control.Monad.RWS.Strict             (ask, get, modify)
import           Control.Monad.Trans                  (liftIO)
import qualified Data.HashMap.Strict                  as Map
import           Data.List                            (foldl')
import           Data.Maybe
import           Foreign.Marshal.Array                (pokeArray)
import           Foreign.Marshal.Alloc                (allocaBytes)
import qualified Graphics.UI.SDL                      as SDL


import Render.Misc                                    (curb,genColorData)
import Types
import UI.UIWidgets
import UI.UIOperations


createGUI :: (Map.HashMap UIId (GUIWidget Pioneers), [UIId])
createGUI = (Map.fromList [ (UIId 0, createPanel (0, 0, 0, 0) [UIId 1, UIId 2] 0)
                          , (UIId 1, createContainer (20, 50, 120, 80) [] 1)
                          , (UIId 2, createPanel (100, 140, 0, 0) [UIId 3, UIId 4] 3)
                          , (UIId 3, createContainer (100, 140, 130, 200) [] 4 )
                          , (UIId 4, createButton (30, 200, 60, 175) 2 testMessage)
                          ], [UIId 0])
         
getGUI :: Map.HashMap UIId (GUIWidget Pioneers) -> [GUIWidget Pioneers]
getGUI = Map.elems
{-# INLINE getGUI #-}

getRootIds :: Pioneers [UIId]
getRootIds = do
  state <- get
  return $ state ^. ui.uiRoots

getRoots :: Pioneers [GUIWidget Pioneers]
getRoots = do
  state <- get
  rootIds <- getRootIds
  let hMap = state ^. ui.uiMap
  return $ toGUIAnys hMap rootIds

testMessage :: MouseButton -> w -> Pixel -> Pioneers w
testMessage btn w (x, y) = do
  case btn of
       LeftButton -> liftIO $ putStrLn ("\tleft click on " ++ show x ++ "," ++ show y)
       RightButton -> liftIO $ putStrLn ("\tright click on " ++ show x ++ "," ++ show y)
       MiddleButton -> liftIO $ putStrLn ("\tmiddle click on " ++ show x ++ "," ++ show y)
       MouseX1 -> liftIO $ putStrLn ("\tX1 click on " ++ show x ++ "," ++ show y)
       MouseX2 -> liftIO $ putStrLn ("\tX2 click on " ++ show x ++ "," ++ show y)
  return w

transformButton :: SDL.MouseButton -> Maybe MouseButton
transformButton SDL.LeftButton = Just LeftButton
transformButton SDL.RightButton = Just RightButton
transformButton SDL.MiddleButton = Just MiddleButton
transformButton SDL.MouseX1 = Just MouseX1
transformButton SDL.MouseX2 = Just MouseX2
transformButton _ = Nothing

eventCallback :: SDL.Event -> Pioneers ()
eventCallback e = do
        env <- ask
        case SDL.eventData e of
            SDL.Window _ _ -> -- windowID event
                -- TODO: resize GUI
                return ()
            SDL.Keyboard movement _ _ key -> -- keyMovement windowID keyRepeat keySym
                     -- need modifiers? use "keyModifiers key" to get them
                let aks = keyboard.arrowsPressed in
                case SDL.keyScancode key of
                    SDL.R    ->
                        liftIO $ do
                                r <- SDL.getRenderer $ env ^. windowObject
                                putStrLn $ unwords ["Renderer: ",show r]
                    SDL.Escape   ->
                        modify $ window.shouldClose .~ True
                    SDL.Left  ->
                        modify $ aks.left  .~ (movement == SDL.KeyDown)
                    SDL.Right ->
                        modify $ aks.right .~ (movement == SDL.KeyDown)
                    SDL.Up    ->
                        modify $ aks.up    .~ (movement == SDL.KeyDown)
                    SDL.Down  ->
                        modify $ aks.down  .~ (movement == SDL.KeyDown)
                    SDL.KeypadPlus ->
                        when (movement == SDL.KeyDown) $ do
                            modify $ (gl.glMap.stateTessellationFactor) %~ ((min 5) . (+1))
                            state <- get
                            liftIO $ putStrLn $ unwords ["Tessellation at: ", show $ state ^. gl.glMap.stateTessellationFactor]
                    SDL.KeypadMinus ->
                        when (movement == SDL.KeyDown) $ do
                            modify $ (gl.glMap.stateTessellationFactor) %~ ((max 1) . (+(-1)))
                            state <- get
                            liftIO $ putStrLn $ unwords ["Tessellation at: ", show $ state ^. gl.glMap.stateTessellationFactor]
                    _ ->
                        return ()
            SDL.MouseMotion _ _ _ (SDL.Position x y) _ _ -> -- windowID mouseID motionState motionPosition xrel yrel
                do
                state <- get
                when (state ^. mouse.isDown && not (state ^. mouse.isDragging)) $
                    modify $ (mouse.isDragging .~ True)
                           . (mouse.dragStartX .~ (fromIntegral x))
                           . (mouse.dragStartY .~ (fromIntegral y))
                           . (mouse.dragStartXAngle .~ (state ^. camera.xAngle))
                           . (mouse.dragStartYAngle .~ (state ^. camera.yAngle))

                modify $ (mouse.mousePosition. Types._x .~ (fromIntegral x))
                       . (mouse.mousePosition. Types._y .~ (fromIntegral y))
            SDL.MouseButton _ _ button state (SDL.Position x y) -> -- windowID mouseID button buttonState buttonAt
                case button of
                    SDL.LeftButton -> do
                        let pressed = state == SDL.Pressed
                        modify $ mouse.isDown .~ pressed
                        unless pressed $ do
                            st <- get
                            if st ^. mouse.isDragging then
                                modify $ mouse.isDragging .~ False
                            else
                                clickHandler LeftButton (x, y)
                    _ -> do when (state == SDL.Released)
                                $ maybe (return ()) (`clickHandler` (x, y)) $ transformButton button
            SDL.MouseWheel _ _ _ vscroll -> -- windowID mouseID hScroll vScroll
                do
                state <- get
                let zDist' = (state ^. camera.zDist) + realToFrac (negate vscroll) in
                  modify $ camera.zDist .~ (curb (env ^. zDistClosest) (env ^. zDistFarthest) zDist')
            -- there is more (joystic, touchInterface, ...), but currently ignored
            SDL.Quit -> modify $ window.shouldClose .~ True
            _ ->  liftIO $ putStrLn $ unwords ["Not processing Event:", show e]


-- | Handler for UI-Inputs.
--   Indicates a primary click on something (e.g. left-click, touch on Touchpad, fire on Gamepad, ...
clickHandler :: MouseButton -> Pixel -> Pioneers ()
clickHandler btn pos@(x,y) = do
  state <- get
  let hMap = state ^. ui.uiMap
  roots <- getRootIds
  hits <- liftM concat $ mapM (getInsideId hMap pos) roots
  case hits of
       [] -> liftIO $ putStrLn $ unwords [show btn ++ ":press on (",show x,",",show y,")"]
       _  -> do
         changes <- mapM (\uid -> do
           let w = toGUIAny hMap uid
               short = w ^. baseProperties.shorthand
           bound <- w ^. baseProperties.boundary
           prio <- w ^. baseProperties.priority
           liftIO $ putStrLn $ "hitting " ++ short ++ ": " ++ show bound ++ " " ++ show prio
                            ++ " at [" ++ show x ++ "," ++ show y ++ "]"
           case w ^. mouseActions of
                Just ma -> do w'  <- (ma ^. onMousePress) btn pos w
                              w'' <- (ma ^. onMouseRelease) btn pos w'
                              return $ Just (uid, w'')
                Nothing  -> return Nothing
           ) $ hits
         let newMap :: Map.HashMap UIId (GUIWidget Pioneers)
             newMap = foldl' (\hm (uid, w') -> Map.insert uid w' hm) hMap $ catMaybes changes
         modify $ ui.uiMap .~ newMap
         return ()
         

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
                roots <- getRoots
                let tex = (state ^. gl.glHud.hudTexture)
                liftIO $ do
                    -- bind texture - all later calls work on this one.
                    GL.textureBinding GL.Texture2D GL.$= Just tex
                mapM_ (copyGUI tex) roots
                modify $ ui.uiHasChanged .~ False

--TODO: Perform border-checking ... is xoff + width and yoff+height inside the screen-coordinates..
copyGUI :: GL.TextureObject -> GUIWidget Pioneers -> Pioneers ()
copyGUI tex widget = do
                        (xoff, yoff, wWidth, wHeight) <- widget ^. baseProperties.boundary
                        state <- get
                        let 
                            hMap = state ^. ui.uiMap
                            int = fromInteger.toInteger --conversion between Int8, GLInt, Int, ...
                            --temporary color here. lateron better some getData-function to
                            --get a list of pixel-data or a texture.
                            color = case widget ^. baseProperties.shorthand of
                                "CNT" -> [255,0,0,128]
                                "BTN" -> [255,255,0,255]
                                "PNL" -> [128,128,128,128]
                                _     -> [255,0,255,255]
                        liftIO $ allocaBytes (wWidth*wHeight*4) $ \ptr -> do
                                --copy data into C-Array
                                pokeArray ptr (genColorData (wWidth*wHeight) color)
                                GL.texSubImage2D
                                        GL.Texture2D
                                        0
                                        (GL.TexturePosition2D (int xoff) (int yoff))
                                        (GL.TextureSize2D (int wWidth) (int wHeight))
                                        (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
                        nextChildrenIds <- widget ^. baseProperties.children
                        mapM_ (copyGUI tex) $ toGUIAnys hMap $ nextChildrenIds

--TODO: Add scroll-Handler, return (Pioneers Bool) to indicate event-bubbling etc.
--TODO: Maybe queues are better?