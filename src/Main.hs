{-# LANGUAGE BangPatterns, DoAndIfThenElse #-}
module Main where

-- Monad-foo and higher functional stuff
import           Control.Monad                        (unless, void, when, join)
import Control.Arrow ((***))

-- data consistency/conversion
import           Control.Concurrent                   (threadDelay)
import           Control.Concurrent.STM               (TQueue,
                                                       newTQueueIO)

import           Control.Monad.RWS.Strict             (RWST, ask, asks,
                                                       evalRWST, get, liftIO,
                                                       modify, put)
import           Data.Distributive                    (distribute, collect)

-- FFI
import           Foreign                              (Ptr, castPtr, with)
import           Foreign.C                            (CFloat)

-- Math
import           Control.Lens                         ((^.), (.~), (%~))
import           Linear                               as L

-- GUI
import           Graphics.UI.SDL                      as SDL
--import           Graphics.UI.SDL.TTF                  as TTF
--import           Graphics.UI.SDL.TTF.Types

-- Render
import qualified Graphics.Rendering.OpenGL.GL         as GL
import           Graphics.Rendering.OpenGL.Raw.Core31
import           Data.Time                            (getCurrentTime, diffUTCTime)

import Graphics.Rendering.OpenGL.Raw.ARB.TessellationShader
-- Our modules
import           Map.Map
import           Render.Misc                          (checkError,
                                                       createFrustum, getCam,
                                                       curb)
import           Render.Render                        (initRendering,
                                                       initShader)
import           UI.Callbacks
import           Types

import qualified Debug.Trace                          as D (trace)

--------------------------------------------------------------------------------
main :: IO ()
main = do
        SDL.withInit [InitVideo, InitAudio] $ do --also: InitNoParachute -> faster, without parachute!
        SDL.withWindow "Pioneers" (SDL.Position 100 100) (Size 1024 600) [WindowOpengl     -- we want openGL
                                                                             ,WindowShown      -- window should be visible
                                                                             ,WindowResizable  -- and resizable 
                                                                             ,WindowInputFocus -- focused (=> active)
                                                                             ,WindowMouseFocus -- Mouse into it
                                                                             ,WindowInputGrabbed-- never let go of input (KB/Mouse)
                                                                             ] $ \window -> do
        withOpenGL window $ do
        --TTF.withInit $ do
        (Size fbWidth fbHeight) <- glGetDrawableSize window
        initRendering
        --generate map vertices
        (mapBuffer, vert) <- getMapBufferObject
        (ci, ni, vi, pri, vii, mi, nmi, tli, tlo) <- initShader
        putStrLn "foo"
        eventQueue <- newTQueueIO :: IO (TQueue Event)
        putStrLn "foo"
        now <- getCurrentTime
        putStrLn "foo"
        --font <- TTF.openFont "fonts/ttf-04B_03B_/04B_03B_.TTF" 10
        --TTF.setFontStyle font TTFNormal
        --TTF.setFontHinting font TTFHNormal
        {-winRenderer <- getRenderer window
        hudTex      <- createTexture 
                                       winRenderer            -- where
                                       PixelFormatRGBA8888    -- RGBA32-bit
                                       TextureAccessStreaming -- change occasionally
                                       1024                   -- width
                                       600                    -- height-}

        let zDistClosest  = 1
            zDistFarthest = zDistClosest + 30
            --TODO: Move near/far/fov to state for runtime-changability & central storage
            fov           = 90  --field of view
            near          = 1   --near plane
            far           = 100 --far plane
            ratio         = fromIntegral fbWidth / fromIntegral fbHeight
            frust         = createFrustum fov near far ratio
            aks = ArrowKeyState {
                  _up       = False
                , _down     = False
                , _left     = False
                , _right    = False
            }
            glMap = GLMapState
                { _shdrVertexIndex      = vi
                , _shdrNormalIndex      = ni
                , _shdrColorIndex       = ci
                , _shdrProjMatIndex     = pri
                , _shdrViewMatIndex     = vii
                , _shdrModelMatIndex    = mi
                , _shdrNormalMatIndex   = nmi
                , _shdrTessInnerIndex   = tli
                , _shdrTessOuterIndex   = tlo
                , _stateTessellationFactor = 4
                , _stateMap             = mapBuffer
                , _mapVert              = vert
                }
            env = Env
              { _eventsChan      = eventQueue
              , _windowObject    = window
              , _zDistClosest    = zDistClosest
              , _zDistFarthest   = zDistFarthest
              --, envFont          = font
              }
            state = State
              { _window              = WindowState
                        { _width               = fbWidth
                        , _height              = fbHeight
                        , _shouldClose         = False
                        }
              , _camera              = CameraState
                        { _xAngle              = pi/6
                        , _yAngle              = pi/2
                        , _zDist               = 10
                        , _frustum             = frust
                        , _camPosition         = Types.Position
                                       { Types._x    = 5
                                       , Types._y    = 5
                                       }
                        }
              , _io                  = IOState
                        { _clock               = now
                        }
              , _mouse               = MouseState
                        { _isDown              = False
                        , _isDragging          = False
                        , _dragStartX          = 0
                        , _dragStartY          = 0
                        , _dragStartXAngle     = 0
                        , _dragStartYAngle     = 0
                        , _mousePosition       = Types.Position
                                         { Types._x  = 5
                                         , Types._y  = 5
                                         }
                        }
              , _keyboard            = KeyboardState
                        { _arrowsPressed       = aks
                        }
              , _gl                  = GLState
                        { _glMap               = glMap
                        , _hudTexture          = Nothing
                        }
              , _game                = GameState
                        {
                        }
              }

        putStrLn "init done."
        void $ evalRWST (adjustWindow >> run) env state

        destroyWindow window

-- Render-Pipeline

draw :: Pioneers ()
draw = do
    state <- get
    let xa       = state ^. camera.xAngle
        ya       = state ^. camera.yAngle
        (GL.UniformLocation proj)  = state ^. gl.glMap.shdrProjMatIndex   
        (GL.UniformLocation nmat)  = state ^. gl.glMap.shdrNormalMatIndex 
        (GL.UniformLocation vmat)  = state ^. gl.glMap.shdrViewMatIndex   
        (GL.UniformLocation tli)   = state ^. gl.glMap.shdrTessInnerIndex 
        (GL.UniformLocation tlo)   = state ^. gl.glMap.shdrTessOuterIndex 
        vi       = state ^. gl.glMap.shdrVertexIndex 
        ni       = state ^. gl.glMap.shdrNormalIndex 
        ci       = state ^. gl.glMap.shdrColorIndex  
        numVert  = state ^. gl.glMap.mapVert         
        map'     = state ^. gl.glMap.stateMap        
        frust    = state ^. camera.frustum           
        camX     = state ^. camera.camPosition.x
        camY     = state ^. camera.camPosition.y
        zDist'   = state ^. camera.zDist
        tessFac  = state ^. gl.glMap.stateTessellationFactor
    liftIO $ do
        --(vi,GL.UniformLocation proj) <- initShader
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        checkError "foo"
        --set up projection (= copy from state)
        with (distribute frust) $ \ptr ->
              glUniformMatrix4fv proj 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))
        checkError "foo"

        --set up camera
        let ! cam = getCam (camX,camY) zDist' xa ya
        with (distribute cam) $ \ptr ->
              glUniformMatrix4fv vmat 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))
        checkError "foo"
              
        --set up normal--Mat transpose((model*camera)^-1)
        let normal = (case inv33 (fmap (^. _xyz) cam ^. _xyz) of
                                             (Just a) -> a
                                             Nothing  -> eye3) :: M33 CFloat
            nmap = collect id normal :: M33 CFloat --transpose...
        
        with (distribute nmap) $ \ptr ->
              glUniformMatrix3fv nmat 1 0 (castPtr (ptr :: Ptr (M33 CFloat)))

        checkError "nmat"
        
        glUniform1f tli (fromIntegral tessFac)
        glUniform1f tlo (fromIntegral tessFac)

        GL.bindBuffer GL.ArrayBuffer GL.$= Just map'
        GL.vertexAttribPointer ci GL.$= fgColorIndex
        GL.vertexAttribArray ci   GL.$= GL.Enabled
        GL.vertexAttribPointer ni GL.$= fgNormalIndex
        GL.vertexAttribArray ni   GL.$= GL.Enabled
        GL.vertexAttribPointer vi GL.$= fgVertexIndex
        GL.vertexAttribArray vi   GL.$= GL.Enabled
        checkError "beforeDraw"
        
        glPatchParameteri gl_PATCH_VERTICES 3
        glPolygonMode gl_FRONT gl_LINE

        glDrawArrays gl_PATCHES 0 (fromIntegral numVert)
        checkError "draw"


-- Main game loop

run :: Pioneers ()
run = do
    env <- ask

    -- draw Scene
    draw
    liftIO $ glSwapWindow (env ^. windowObject)
    -- getEvents & process
    processEvents

    -- update State

    state <- get
    -- change in camera-angle
    when (state ^. mouse.isDragging) $ do
          let sodx  = state ^. mouse.dragStartX
              sody  = state ^. mouse.dragStartY
              sodxa = state ^. mouse.dragStartXAngle
              sodya = state ^. mouse.dragStartYAngle
              x'    = state ^. mouse.mousePosition.x
              y'    = state ^. mouse.mousePosition.y
              myrot = (x' - sodx) / 2
              mxrot = (y' - sody) / 2
              newXAngle  = curb (pi/12) (0.45*pi) newXAngle'
              newXAngle' = sodxa + mxrot/100
              newYAngle
                  | newYAngle' > pi    = newYAngle' - 2 * pi
                  | newYAngle' < (-pi) = newYAngle' + 2 * pi
                  | otherwise          = newYAngle'
              newYAngle' = sodya + myrot/100
          
          modify $ ((camera.xAngle) .~ newXAngle)
                 . ((camera.yAngle) .~ newYAngle)

    -- get cursor-keys - if pressed
    --TODO: Add sin/cos from stateYAngle
    (kxrot, kyrot) <- fmap (join (***) fromIntegral) getArrowMovement
    let 
        multc = cos $ state ^. camera.yAngle
        mults = sin $ state ^. camera.yAngle
        modx x' = x' - 0.2 * kxrot * multc
                     - 0.2 * kyrot * mults
        mody y' = y' - 0.2 * kxrot * mults
                     - 0.2 * kyrot * multc
    modify $ (camera.camPosition.x %~ modx)
           . (camera.camPosition.y %~ mody)

    {-
    --modify the state with all that happened in mt time.
    mt <- liftIO GLFW.getTime
    modify $ \s -> s
      {
      }
    -}

    mt <- liftIO $ do
        now <- getCurrentTime
        diff <- return $ diffUTCTime now (state ^. io.clock) -- get time-diffs
        title <- return $ unwords ["Pioneers @ ",show ((round .fromRational.toRational $ 1.0/diff)::Int),"fps"]
        setWindowTitle (env ^. windowObject) title
        sleepAmount <- return $ floor (max 0 (0.04 - diff))*1000000 -- get time until next frame in microseconds
        threadDelay sleepAmount
        return now
    -- set state with new clock-time
    modify $ io.clock .~ mt
    shouldClose <- return $ state ^. window.shouldClose
    unless shouldClose run

getArrowMovement :: Pioneers (Int, Int)
getArrowMovement = do
        state <- get
        aks <- return $ state ^. (keyboard.arrowsPressed) 
        let 
                horz   = left' + right'
                vert   = up'+down'
                left'  = if aks ^. left  then -1 else 0
                right' = if aks ^. right then  1 else 0
                up'    = if aks ^. up    then -1 else 0
                down'  = if aks ^. down  then  1 else 0
        return (horz,vert)

adjustWindow :: Pioneers ()
adjustWindow = do
    state <- get
    let fbWidth  = state ^. window.width
        fbHeight = state ^. window.height
        fov           = 90  --field of view
        near          = 1   --near plane
        far           = 100 --far plane
        ratio         = fromIntegral fbWidth / fromIntegral fbHeight
        frust         = createFrustum fov near far ratio
    liftIO $ glViewport 0 0 (fromIntegral fbWidth) (fromIntegral fbHeight)
    modify $ camera.frustum .~ frust


processEvents :: Pioneers ()
processEvents = do
    me <- liftIO pollEvent
    case me of
      Just e -> do
          processEvent e
          processEvents
      Nothing -> return ()

processEvent :: Event -> Pioneers ()
processEvent e = do
        return ()
        case eventData e of
            Window _ winEvent ->
                case winEvent of
                    Closing ->
                            modify $ window.shouldClose .~ True
                    Resized {windowResizedTo=size} -> do
                            modify $ (window.width  .~ (sizeWidth  size))
                                   . (window.height .~ (sizeHeight size))
                            adjustWindow
                    SizeChanged ->
                            adjustWindow
                    _ ->
                        return ()
                        --liftIO $ putStrLn $ unwords ["Unhandled Window-Event:",show e]
            Keyboard movement _ isRepeated key -> --up/down window(ignored) true/false actualKey
                     -- need modifiers? use "keyModifiers key" to get them
                let aks = keyboard.arrowsPressed in
                case keyScancode key of
                    Escape   ->
                        modify $ window.shouldClose .~ True
                    SDL.Left  ->
                        modify $ aks.left  .~ (movement == KeyDown)
                    SDL.Right ->
                        modify $ aks.right .~ (movement == KeyDown)
                    SDL.Up    ->
                        modify $ aks.up    .~ (movement == KeyDown)
                    SDL.Down  ->
                        modify $ aks.down  .~ (movement == KeyDown)
                    SDL.KeypadPlus ->
                        when (movement == KeyDown) $ do
                            modify $ (gl.glMap.stateTessellationFactor) %~ ((min 5) . (+1))
                            state <- get
                            liftIO $ putStrLn $ unwords ["Tessellation at: ", show $ state ^. gl.glMap.stateTessellationFactor]
                    SDL.KeypadMinus ->
                        when (movement == KeyDown) $ do
                            modify $ (gl.glMap.stateTessellationFactor) %~ ((max 1) . (+(-1)))
                            state <- get
                            liftIO $ putStrLn $ unwords ["Tessellation at: ", show $ state ^. gl.glMap.stateTessellationFactor]
                    _ ->
                        return ()
            MouseMotion _ mouseId st (SDL.Position x y) xrel yrel -> do
                state <- get
                when (state ^. mouse.isDown && not (state ^. mouse.isDragging)) $
                    modify $ (mouse.isDragging .~ True)
                           . (mouse.dragStartX .~ (fromIntegral x))
                           . (mouse.dragStartY .~ (fromIntegral y))
                           . (mouse.dragStartXAngle .~ (state ^. camera.xAngle))
                           . (mouse.dragStartYAngle .~ (state ^. camera.yAngle))
                    
                modify $ (mouse.mousePosition. Types.x .~ (fromIntegral x))
                       . (mouse.mousePosition. Types.y .~ (fromIntegral y))
            MouseButton _ mouseId button state (SDL.Position x y) ->
                case button of
                    LeftButton -> do
                        let pressed = state == Pressed
                        modify $ mouse.isDown .~ pressed
                        unless pressed $ do
                            st <- get
                            if st ^. mouse.isDragging then
                                modify $ mouse.isDragging .~ False
                            else
                                clickHandler (UI.Callbacks.Pixel x y)
                    RightButton -> do
                        when (state == Released) $ alternateClickHandler (UI.Callbacks.Pixel x y)
                    _ ->
                        return ()
            MouseWheel _ mouseId hscroll vscroll -> do
                env <- ask
                state <- get
                let zDist' = (state ^. camera.zDist) + realToFrac (negate vscroll) in 
                  modify $ camera.zDist .~ (curb (env ^. zDistClosest) (env ^. zDistFarthest) zDist')
            Quit -> modify $ window.shouldClose .~ True
            -- there is more (joystic, touchInterface, ...), but currently ignored
            _ ->  liftIO $ putStrLn $ unwords ["Not processing Event:", show e]
