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
import           Control.Lens                         ((^.))
import           Linear                               as L

-- GUI
import           Graphics.UI.SDL                      as SDL hiding (Position)
--import           Graphics.UI.SDL.TTF                  as TTF
--import           Graphics.UI.SDL.TTF.Types

-- Render
import qualified Graphics.Rendering.OpenGL.GL         as GL
import           Graphics.Rendering.OpenGL.Raw.Core31
import           Data.Time                            (getCurrentTime, UTCTime, diffUTCTime)

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
        SDL.withWindow "Pioneers" (Position 100 100) (Size 1024 600) [WindowOpengl     -- we want openGL
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
                        , _camPosition         = Position
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
                        , _mousePosition       = Position
                                         { Types._x  = 5
                                         , Types._y  = 5
                                         }
                        }
              , _keyboard            = KeyboardState
                        { _arrowsPressed       = aks
                        }
              , _gl                  = GLState
                        { _glMap               = glMap
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
    let xa       = get (camera . xAngle) state --stateXAngle          state
        ya       = stateYAngle          state
        (GL.UniformLocation proj)  = shdrProjMatIndex   state
        (GL.UniformLocation nmat)  = shdrNormalMatIndex state
        (GL.UniformLocation vmat)  = shdrViewMatIndex   state
        (GL.UniformLocation tli)   = shdrTessInnerIndex state
        (GL.UniformLocation tlo)   = shdrTessOuterIndex state
        vi       = shdrVertexIndex      state
        ni       = shdrNormalIndex      state
        ci       = shdrColorIndex       state
        numVert  = mapVert              state
        map'     = stateMap             state
        frust    = stateFrustum         state
        camX     = statePositionX       state
        camY     = statePositionY       state
        zDist    = stateZDist           state
        tessFac  = stateTessellationFactor state
    liftIO $ do
        --(vi,GL.UniformLocation proj) <- initShader
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        checkError "foo"
        --set up projection (= copy from state)
        with (distribute frust) $ \ptr ->
              glUniformMatrix4fv proj 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))
        checkError "foo"

        --set up camera
        let ! cam = getCam (camX,camY) zDist xa ya
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
    win <- asks envWindow

    -- draw Scene
    draw
    liftIO $ glSwapWindow win
    -- getEvents & process
    processEvents

    -- update State

    state <- get
    -- change in camera-angle
    when (stateDragging state) $ do
          let sodx  = stateDragStartX      state
              sody  = stateDragStartY      state
              sodxa = stateDragStartXAngle state
              sodya = stateDragStartYAngle state
              x     = stateCursorPosX      state
              y     = stateCursorPosY      state
          let myrot = (x - sodx) / 2
              mxrot = (y - sody) / 2
              newXAngle  = curb (pi/12) (0.45*pi) newXAngle'
              newXAngle' = sodxa + mxrot/100
              newYAngle
                  | newYAngle' > pi    = newYAngle' - 2 * pi
                  | newYAngle' < (-pi) = newYAngle' + 2 * pi
                  | otherwise          = newYAngle'
              newYAngle' = sodya + myrot/100
          put $ state
            { stateXAngle = newXAngle
            , stateYAngle = newYAngle
            }

    -- get cursor-keys - if pressed
    --TODO: Add sin/cos from stateYAngle
    (kxrot, kyrot) <- fmap (join (***) fromIntegral) getArrowMovement
    modify $ \s -> 
                   let 
                        multc = cos $ stateYAngle s
                        mults = sin $ stateYAngle s
                   in 
                   s {
                        statePositionX = statePositionX s - 0.2 * kxrot * multc
                                                          - 0.2 * kyrot * mults
                     ,  statePositionY = statePositionY s + 0.2 * kxrot * mults
                                                          - 0.2 * kyrot * multc
                     }
    
    {-
    --modify the state with all that happened in mt time.
    mt <- liftIO GLFW.getTime
    modify $ \s -> s
      {
      }
    -}
    mt <- liftIO $ do
        now <- getCurrentTime
        diff <- return $ diffUTCTime now (stateClock state) -- get time-diffs
        title <- return $ unwords ["Pioneers @ ",show ((round .fromRational.toRational $ 1.0/diff)::Int),"fps"]
        setWindowTitle win title
        sleepAmount <- return $ floor (max 0 (0.04 - diff))*1000000 -- get time until next frame in microseconds
        threadDelay sleepAmount
        return now
    -- set state with new clock-time
    modify $ \s -> s
        {
                stateClock = mt
        }
    shouldClose <- return $ stateWinClose state
    unless shouldClose run

getArrowMovement :: Pioneers (Int, Int)
getArrowMovement = do
        state <- get
        aks <- return $ stateArrowsPressed state
        let 
                horz   = left' + right'
                vert   = up'+down'
                left'  = if arrowLeft aks  then -1 else 0
                right' = if arrowRight aks then  1 else 0
                up'    = if arrowUp aks    then -1 else 0
                down'  = if arrowDown aks  then  1 else 0
        return (horz,vert)

adjustWindow :: Pioneers ()
adjustWindow = do
    state <- get
    let fbWidth  = stateWindowWidth  state
        fbHeight = stateWindowHeight state
        fov           = 90  --field of view
        near          = 1   --near plane
        far           = 100 --far plane
        ratio         = fromIntegral fbWidth / fromIntegral fbHeight
        frust         = createFrustum fov near far ratio
    liftIO $ glViewport 0 0 (fromIntegral fbWidth) (fromIntegral fbHeight)
    put $ state {
        stateFrustum = frust
    }


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
        case eventData e of
            Window _ winEvent ->
                case winEvent of
                    Closing ->
                            modify $ \s -> s {
                                stateWinClose = True
                            }
                    Resized {windowResizedTo=size} -> do
                            modify $ \s -> s {
                                stateWindowWidth  = sizeWidth  size
                               ,stateWindowHeight = sizeHeight size
                            }
                            adjustWindow
                    SizeChanged ->
                            adjustWindow
                    _ ->
                        return ()
                        --liftIO $ putStrLn $ unwords ["Unhandled Window-Event:",show e]
            Keyboard movement _ isRepeated key -> --up/down window(ignored) true/false actualKey
                     -- need modifiers? use "keyModifiers key" to get them
                case keyScancode key of
                    Escape   ->
                        modify $ \s -> s {
                            stateWinClose = True
                        }
                    SDL.Left  ->
                        modify $ \s -> s {
                            stateArrowsPressed = (stateArrowsPressed s) {
                                    arrowLeft = movement == KeyDown
                                }
                            }
                    SDL.Right ->
                        modify $ \s -> s {
                            stateArrowsPressed = (stateArrowsPressed s) {
                                    arrowRight = movement == KeyDown
                                }
                            }
                    SDL.Up    ->
                        modify $ \s -> s {
                            stateArrowsPressed = (stateArrowsPressed s) {
                                    arrowUp = movement == KeyDown
                                 }
                            }
                    SDL.Down  ->
                        modify $ \s -> s {
                            stateArrowsPressed = (stateArrowsPressed s) {
                                    arrowDown = movement == KeyDown
                                }
                            }
                    SDL.KeypadPlus ->
                        when (movement == KeyDown) $ do
                            modify $ \s -> s {
                                stateTessellationFactor = min (stateTessellationFactor s + 1) 5
                            }
                            state <- get
                            liftIO $ putStrLn $ unwords ["Tessellation at: ", show $ stateTessellationFactor state]
                    SDL.KeypadMinus ->
                        when (movement == KeyDown) $ do
                            modify $ \s -> s {
                                stateTessellationFactor = max (stateTessellationFactor s - 1) 1
                            }
                            state <- get
                            liftIO $ putStrLn $ unwords ["Tessellation at: ", show $ stateTessellationFactor state]
                    _ ->
                        return ()
            MouseMotion _ mouseId st (Position x y) xrel yrel -> do
                state <- get
                when (stateMouseDown state && not (stateDragging state)) $
                    put $ state
                    { stateDragging        = True
                    , stateDragStartX      = fromIntegral x
                    , stateDragStartY      = fromIntegral y
                    , stateDragStartXAngle = stateXAngle state
                    , stateDragStartYAngle = stateYAngle state
                    }
                modify $ \s -> s {
                      stateCursorPosX      = fromIntegral x
                    , stateCursorPosY      = fromIntegral y
                }
            MouseButton _ mouseId button state (Position x y) ->
                case button of
                    LeftButton -> do
                        let pressed = state == Pressed
                        modify $ \s -> s {
                            stateMouseDown = pressed
                        }
                        unless pressed $ do
                            st <- get
                            if stateDragging st then
                                modify $ \s -> s {
                                    stateDragging = False
                                }
                            else
                                clickHandler (UI.Callbacks.Pixel x y)
                    RightButton -> do
                        when (state == Released) $ alternateClickHandler (UI.Callbacks.Pixel x y)
                    _ ->
                        return ()
            MouseWheel _ mouseId hscroll vscroll -> do
                env <- ask
                modify $ \s -> s
                    { stateZDist =
                        let zDist' = stateZDist s + realToFrac (negate vscroll)
                        in curb (envZDistClosest env) (envZDistFarthest env) zDist'
                    }
            Quit -> modify $ \s -> s {stateWinClose = True}
            -- there is more (joystic, touchInterface, ...), but currently ignored
            _ ->  liftIO $ putStrLn $ unwords ["Not processing Event:", show e]
