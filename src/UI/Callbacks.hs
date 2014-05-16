{-# LANGUAGE DoAndIfThenElse #-}
module UI.Callbacks where


import qualified Graphics.Rendering.OpenGL.GL         as GL
import           Control.Lens                         ((^.), (.~), (%~), (^?), at)
import           Control.Monad                        (liftM, when, unless)
import           Control.Monad.RWS.Strict             (ask, get, modify)
import           Control.Monad.Trans                  (liftIO)
import qualified Data.HashMap.Strict                  as Map
import           Data.List                            (foldl')
import           Data.Maybe
import           Foreign.Marshal.Array                (pokeArray)
import           Foreign.Marshal.Alloc                (allocaBytes)
import qualified Graphics.UI.SDL                      as SDL
import           Control.Concurrent.STM.TMVar         (readTMVar, takeTMVar, putTMVar)
import           Control.Concurrent.STM               (atomically)


import Render.Misc                                    (curb,genColorData)
import Types
import UI.UIWidgets
import UI.UIOperations

-- TODO: define GUI positions in a file
createGUI :: (Map.HashMap UIId (GUIWidget Pioneers), [UIId])
createGUI = (Map.fromList [ (UIId 0, createPanel (0, 0, 0, 0) [UIId 1, UIId 2] 0)
                          , (UIId 1, createContainer (30, 215, 100, 80) [] 1)
                          , (UIId 2, createPanel (50, 40, 0, 0) [UIId 3, UIId 4] 3)
                          , (UIId 3, createContainer (80, 15, 130, 90) [] 4 )
                          , (UIId 4, createButton (10, 40, 60, 130) 2 testMessage)
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
                            modify $ gl.glMap.stateTessellationFactor %~ (min 5) . (+1)
                            state <- get
                            liftIO $ putStrLn $ unwords ["Tessellation at: ", show $ state ^. gl.glMap.stateTessellationFactor]
                    SDL.KeypadMinus ->
                        when (movement == SDL.KeyDown) $ do
                            modify $ gl.glMap.stateTessellationFactor %~ (max 1) . (+(-1))
                            state <- get
                            liftIO $ putStrLn $ unwords ["Tessellation at: ", show $ state ^. gl.glMap.stateTessellationFactor]
                    _ ->
                        return ()
            SDL.MouseMotion _ _ _ (SDL.Position x y) _ _ -> -- windowID mouseID motionState motionPosition xrel yrel
                do
                state <- get
                if state ^. mouse.isDown && not (state ^. mouse.isDragging)
                  then
                    do
                    cam <- liftIO $ atomically $ readTMVar (state ^. camera)
                    modify $ (mouse.isDragging .~ True)
                           . (mouse.dragStartX .~ fromIntegral x)
                           . (mouse.dragStartY .~ fromIntegral y)
                           . (mouse.dragStartXAngle .~ (cam ^. xAngle))
                           . (mouse.dragStartYAngle .~ (cam ^. yAngle))
                    else mouseMoveHandler (x, y)
                modify $ (mouse.mousePosition. Types._x .~ fromIntegral x)
                       . (mouse.mousePosition. Types._y .~ fromIntegral y)
            SDL.MouseButton _ _ button state (SDL.Position x y) -> -- windowID mouseID button buttonState buttonAt
             do 
                case button of
                     SDL.LeftButton -> do
                         let pressed = state == SDL.Pressed
                         modify $ mouse.isDown .~ pressed
                         if pressed 
                           then mouseReleaseHandler LeftButton (x, y)
                           else do
                             st <- get
                             if st ^. mouse.isDragging then
                                 modify $ mouse.isDragging .~ False
                             else do
                                 mousePressHandler LeftButton (x, y)
                     _ -> case state of
                               SDL.Pressed -> maybe (return ()) (`mousePressHandler` (x, y)) $ transformButton button
                               SDL.Released -> maybe (return ()) (`mouseReleaseHandler` (x, y)) $ transformButton button
                               _ -> return ()
            SDL.MouseWheel _ _ _ vscroll -> -- windowID mouseID hScroll vScroll
                do
                state <- get
                liftIO $ atomically $ do
                    cam <- takeTMVar (state ^. camera)
                    let zDist' = (cam ^. zDist) + realToFrac (negate vscroll)
                        zDist'' = curb (env ^. zDistClosest) (env ^. zDistFarthest) zDist'
                    cam' <- return $ zDist .~ zDist'' $ cam
                    putTMVar (state ^. camera) cam'
                  
            -- there is more (joystic, touchInterface, ...), but currently ignored
            SDL.Quit -> modify $ window.shouldClose .~ True
            _ ->  liftIO $ putStrLn $ unwords ["Not processing Event:", show e]


mouseButtonHandler :: (EventHandler Pioneers -> MouseButton -> Pixel -> GUIWidget Pioneers -> Pioneers (GUIWidget Pioneers))
                   -> MouseButton -> Pixel -> Pioneers ()
mouseButtonHandler transFunc btn px = do
    state <- get
    let hMap = state ^. ui.uiMap
        currentWidget = state ^. ui.uiButtonState.mouseCurrentWidget
    case currentWidget of
         Just (wid, px') -> do
             let target = toGUIAny hMap wid
             target' <- case target ^. eventHandlers.(at MouseEvent) of
                             Just ma -> transFunc ma btn (px -: px') target
                             Nothing  -> return target
             modify $ ui.uiMap %~ Map.insert wid target'
             return ()
         Nothing -> return ()
         
mousePressHandler :: MouseButton -> Pixel -> Pioneers ()
mousePressHandler btn px = do
    modify $ ui.uiButtonState %~ (mousePressed %~ (+1)) -- TODO: what happens if released outside window? not reset properly?
    mouseButtonHandler (\ma -> fromJust (ma ^? onMousePress)) btn px

mouseReleaseHandler :: MouseButton -> Pixel -> Pioneers ()
mouseReleaseHandler btn px = do
    modify $ ui.uiButtonState %~ (mousePressed %~ flip (-) 1) -- TODO: what happens if pressed outside window? not set properly?
    mouseButtonHandler (\ma -> fromJust (ma ^? onMouseRelease)) btn px
    state <- get
    unless (state ^. ui.uiButtonState.mousePressed > 0) $ do
      case state ^. ui.uiButtonState.mouseCurrentWidget of
           Just (wid, px') -> do
               let target = toGUIAny (state ^. ui.uiMap) wid
               -- debug
               let short = target ^. baseProperties.shorthand
               bound <- target ^. baseProperties.boundary
               prio <- target ^. baseProperties.priority
               liftIO $ putStrLn $ "releasing(" ++ show btn ++ ") " ++ short ++ ": " ++ show bound ++ " "
                                ++ show prio ++ " at [" ++ show (fst px) ++ "," ++ show (snd px) ++ "]"
               -- /debug
               target' <- case target ^. eventHandlers.(at MouseMotionEvent) of --existing handler?
                               Just ma -> do
                                    target_ <- fromJust (ma ^? onMouseEnter) px' target -- TODO unsafe fromJust
                                    fromJust (ma ^? onMouseMove) px' target_ -- TODO unsafe fromJust
                               Nothing  -> return target
               modify $ ui.uiMap %~ Map.insert wid target'
           Nothing -> return ()
      mouseSetMouseActive px -- TODO leave current

mouseSetMouseActiveTargeted :: (UIId, Pixel) -- ^ (target widget, local coorinates)
                               -> Pixel         -- ^ global coordinates
                               -> Pioneers ()
mouseSetMouseActiveTargeted (wid, px') px = do
    state <- get
    --liftIO $ putStrLn $ "new target: " ++ show wid
    let hMap = state ^. ui.uiMap
        target = toGUIAny hMap wid
    modify $ ui.uiButtonState %~ (mouseCurrentWidget .~ Just (wid, px -: px')) . (mouseInside .~ True)
    target' <- case target ^. eventHandlers.(at MouseMotionEvent) of --existing handler?
                    Just ma -> do
                         target_ <- fromJust (ma ^? onMouseEnter) px' target -- TODO unsafe fromJust
                         fromJust (ma ^? onMouseMove) px' target_ -- TODO unsafe fromJust
                    Nothing  -> return target
    modify $ ui.uiMap %~ Map.insert wid target'
    
mouseSetMouseActive :: Pixel -- ^global coordinates
                       -> Pioneers ()
mouseSetMouseActive px = do
    roots <- getRootIds
    hits <- liftM concat $ mapM (getInsideId px) roots
    leading <- getLeadingWidget hits
    case leading of
         Just hit -> mouseSetMouseActiveTargeted hit px
         Nothing -> modify $ ui.uiButtonState %~ (mouseCurrentWidget .~ Nothing) . (mouseInside .~ False)
        
mouseSetLeaving :: UIId -> Pixel -> Pioneers ()
mouseSetLeaving wid px = do
    state <- get
    let target = toGUIAny (state ^. ui.uiMap) wid
    modify $ ui.uiButtonState.mouseInside .~ False
    case target ^. eventHandlers.(at MouseMotionEvent) of --existing handler?
         Just ma -> do
             target' <- fromJust (ma ^? onMouseLeave) px target --TODO unsafe fromJust
             modify $ ui.uiMap %~ Map.insert wid target'
         Nothing -> return ()
        
mouseMoveHandler :: Pixel -> Pioneers ()
mouseMoveHandler px = do
    state <- get
    --liftIO $ print $ state ^. ui.uiButtonState
    case state ^. ui.uiButtonState.mouseCurrentWidget of -- existing mouse-active widget?
         Just (wid, px') -> do
             let target = toGUIAny (state ^. ui.uiMap) wid
             inTest <- isHittingChild (px -: px') target
             case inTest of
                  Left b -> -- no child hit
                      if b == state ^. ui.uiButtonState.mouseInside then -- > moving inside or outside
                        case target ^. eventHandlers.(at MouseMotionEvent) of --existing handler?
                             Just ma -> do target' <- fromJust (ma ^? onMouseMove) px' target
                                           modify $ ui.uiMap %~ Map.insert wid target'
                             Nothing -> return () 
                      else if b then -- && not mouseInside --> entering
                        do modify $ ui.uiButtonState.mouseInside .~ True
                           case target ^. eventHandlers.(at MouseMotionEvent) of --existing handler?
                                Just ma -> do
                                    target_ <- fromJust (ma ^? onMouseEnter) (px -: px') target --TODO unsafe fromJust
                                    target' <- fromJust (ma ^? onMouseMove) (px -: px') target_ --TODO unsafe fromJust
                                    modify $ ui.uiMap %~ Map.insert wid target'
                                Nothing -> return ()
                      else -- not b && mouseInside --> leaving
                        do mouseSetLeaving wid (px -: px')
                           when (state ^. ui.uiButtonState.mousePressed <= 0)  -- change mouse-active widget?
                               $ mouseSetMouseActive px

                  Right childHit -> do
                      mouseSetLeaving wid (px -: px')
                      when (state ^. ui.uiButtonState.mousePressed <= 0)  -- change mouse-active widget?
                          $ mouseSetMouseActiveTargeted childHit px
         Nothing -> do
             mouseSetMouseActive px
             

-- | Handler for UI-Inputs.
--   Indicates a primary click on something (e.g. left-click, touch on Touchpad, fire on Gamepad, ...
clickHandler :: MouseButton -> Pixel -> Pioneers ()
clickHandler btn pos@(x,y) = do
  roots <- getRootIds
  hits <- liftM concat $ mapM (getInsideId pos) roots
  case hits of
       [] -> liftIO $ putStrLn $ unwords [show btn ++ ":press on (",show x,",",show y,")"]
       _  -> do
         changes <- mapM (\(uid, pos') -> do
           state <- get
           let w = toGUIAny (state ^. ui.uiMap) uid
               short = w ^. baseProperties.shorthand
           bound <- w ^. baseProperties.boundary
           prio <- w ^. baseProperties.priority
           liftIO $ putStrLn $ "hitting(" ++ show btn ++ ") " ++ short ++ ": " ++ show bound ++ " "
                             ++ show prio ++ " at [" ++ show x ++ "," ++ show y ++ "]"
           case w ^. eventHandlers.(at MouseEvent) of
                Just ma -> do w'  <- fromJust (ma ^? onMousePress) btn pos' w -- TODO unsafe fromJust
                              w'' <- fromJust (ma ^? onMouseRelease) btn pos' w' -- TODO unsafe fromJust
                              return $ Just (uid, w'')
                Nothing  -> return Nothing
           ) hits
         state <- get
         let hMap = state ^. ui.uiMap
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
                let tex = state ^. gl.glHud.hudTexture
                liftIO $ do
                    -- bind texture - all later calls work on this one.
                    GL.textureBinding GL.Texture2D GL.$= Just tex
                mapM_ (copyGUI tex (0, 0)) roots
                modify $ ui.uiHasChanged .~ False

--TODO: Perform border-checking ... is xoff + width and yoff+height inside the screen-coordinates..
copyGUI :: GL.TextureObject -> Pixel -- ^current view's offset
        -> GUIWidget Pioneers -- ^the widget to draw
        -> Pioneers ()
copyGUI tex (vX, vY) widget = do
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
                                        (GL.TexturePosition2D (int (vX + xoff)) (int (vY + yoff)))
                                        (GL.TextureSize2D (int wWidth) (int wHeight))
                                        (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
                        nextChildrenIds <- widget ^. baseProperties.children
                        mapM_ (copyGUI tex (vX+xoff, vY+yoff)) $ toGUIAnys hMap nextChildrenIds

--TODO: Add scroll-Handler, return (Pioneers Bool) to indicate event-bubbling etc.
--TODO: Maybe queues are better?
