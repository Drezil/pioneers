{-# LANGUAGE BangPatterns #-}
module Main where

-- Monad-foo and higher functional stuff
import           Control.Applicative
import           Control.Monad                        (unless, void, when, join)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import Control.Arrow ((***))

-- data consistency/conversion
import           Control.Concurrent                   (threadDelay)
import           Control.Concurrent.STM               (TQueue, atomically,
                                                       newTQueueIO,
                                                       tryReadTQueue,
                                                       writeTQueue, isEmptyTQueue,
                                                       STM)
import           Control.Monad.RWS.Strict             (RWST, ask, asks,
                                                       evalRWST, get, liftIO,
                                                       modify, put)
import           Data.Distributive                    (distribute, collect)

-- FFI
import           Foreign                              (Ptr, castPtr, with)
import           Foreign.C                            (CFloat)

-- Math
import           Control.Lens                         (transposeOf, (^.))
import           Linear                               as L

-- GUI
import           Graphics.UI.SDL                      as SDL

-- Render
import qualified Graphics.Rendering.OpenGL.GL         as GL
import           Graphics.Rendering.OpenGL.Raw.Core31
import           Data.Time                            (getCurrentTime, UTCTime, diffUTCTime)

-- Our modules
import           Map.Map
import           Render.Misc                          (checkError,
                                                       createFrustum, getCam,
                                                       lookAt, up, curb)
import           Render.Render                        (initRendering,
                                                       initShader)

import qualified Debug.Trace                          as D (trace)

data ArrowKeyState = ArrowKeyState {
         arrowUp      :: !Bool
        ,arrowDown    :: !Bool
        ,arrowLeft    :: !Bool
        ,arrowRight   :: !Bool
}

--Static Read-Only-State
data Env = Env
    { envEventsChan    :: TQueue Event
    , envWindow        :: !Window
    , envZDistClosest  :: !Double
    , envZDistFarthest :: !Double
    --, envGLContext     :: !GLContext
    }

--Mutable State
data State = State
    { stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    , stateWinClose        :: !Bool
    , stateClock           :: !UTCTime
    --- IO
    , stateXAngle          :: !Double
    , stateYAngle          :: !Double
    , stateZDist           :: !Double
    , stateMouseDown       :: !Bool
    , stateDragging        :: !Bool
    , stateDragStartX      :: !Double
    , stateDragStartY      :: !Double
    , stateDragStartXAngle :: !Double
    , stateDragStartYAngle :: !Double
    , statePositionX       :: !Double
    , statePositionY       :: !Double
    , stateCursorPosX      :: !Double
    , stateCursorPosY      :: !Double
    , stateArrowsPressed   :: !ArrowKeyState
    , stateFrustum         :: !(M44 CFloat)
    --- pointer to bindings for locations inside the compiled shader
    --- mutable because shaders may be changed in the future.
    , shdrVertexIndex      :: !GL.AttribLocation
    , shdrColorIndex       :: !GL.AttribLocation
    , shdrNormalIndex      :: !GL.AttribLocation
    , shdrProjMatIndex     :: !GL.UniformLocation
    , shdrViewMatIndex     :: !GL.UniformLocation
    , shdrModelMatIndex    :: !GL.UniformLocation
    , shdrNormalMatIndex   :: !GL.UniformLocation
    --- the map
    , stateMap             :: !GL.BufferObject
    , mapVert              :: !GL.NumArrayIndices
    }

type Pioneers = RWST Env () State IO

--------------------------------------------------------------------------------
main :: IO ()
main = do
        SDL.withInit [InitVideo, InitAudio] $ do --also: InitNoParachute -> faster, without parachute!
        SDL.withWindow "Pioneers" (Position 1500 100) (Size 1024 768) [WindowOpengl     -- we want openGL
                                                                             ,WindowShown      -- window should be visible
                                                                             ,WindowResizable  -- and resizable 
                                                                             ,WindowInputFocus -- focused (=> active)
                                                                             ,WindowMouseFocus -- Mouse into it
                                                                             --,WindowInputGrabbed-- never let go of input (KB/Mouse)
                                                                             ] $ \window -> do
        withOpenGL window $ do
        (Size fbWidth fbHeight) <- glGetDrawableSize window
        initRendering
        --generate map vertices
        (mapBuffer, vert) <- getMapBufferObject
        (ci, ni, vi, pri, vii, mi, nmi) <- initShader
        putStrLn "foo"
        eventQueue <- newTQueueIO :: IO (TQueue Event)
        putStrLn "foo"
        now <- getCurrentTime
        putStrLn "foo"

        let zDistClosest  = 10
            zDistFarthest = zDistClosest + 20
            fov           = 90  --field of view
            near          = 1   --near plane
            far           = 100 --far plane
            ratio         = fromIntegral fbWidth / fromIntegral fbHeight
            frust         = createFrustum fov near far ratio
            aks = ArrowKeyState {
                 arrowUp       = False
                ,arrowDown     = False
                ,arrowLeft     = False
                ,arrowRight    = False
            }
            env = Env
              { envEventsChan    = eventQueue
              , envWindow        = window
              , envZDistClosest  = zDistClosest
              , envZDistFarthest = zDistFarthest
              }
            state = State
              { stateWindowWidth     = fbWidth
              , stateWindowHeight    = fbHeight
              , stateXAngle          = pi/6
              , stateYAngle          = pi/2
              , stateZDist           = 10
              , statePositionX       = 5
              , statePositionY       = 5
              , stateCursorPosX      = 0
              , stateCursorPosY      = 0
              , stateMouseDown       = False
              , stateDragging        = False
              , stateDragStartX      = 0
              , stateDragStartY      = 0
              , stateDragStartXAngle = 0
              , stateDragStartYAngle = 0
              , shdrVertexIndex      = vi
              , shdrNormalIndex      = ni
              , shdrColorIndex       = ci
              , shdrProjMatIndex     = pri
              , shdrViewMatIndex     = vii
              , shdrModelMatIndex    = mi
              , shdrNormalMatIndex   = nmi
              , stateMap             = mapBuffer
              , mapVert              = vert
              , stateFrustum         = frust
              , stateWinClose        = False
              , stateClock           = now
              , stateArrowsPressed   = aks
              }

        putStrLn "init done."
        void $ evalRWST (adjustWindow >> run) env state

        destroyWindow window

-- Render-Pipeline

draw :: Pioneers ()
draw = do
    env   <- ask
    state <- get
    let xa       = stateXAngle          state
        ya       = stateYAngle          state
        (GL.UniformLocation proj)  = shdrProjMatIndex   state
        (GL.UniformLocation nmat)  = shdrNormalMatIndex state
        (GL.UniformLocation vmat)  = shdrViewMatIndex   state
        vi       = shdrVertexIndex      state
        ni       = shdrNormalIndex      state
        ci       = shdrColorIndex       state
        numVert  = mapVert              state
        map'     = stateMap             state
        frust    = stateFrustum         state
        camX     = statePositionX       state
        camY     = statePositionY       state
        zDist    = stateZDist           state
    liftIO $ do
        --(vi,GL.UniformLocation proj) <- initShader
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        checkError "foo"
        --set up projection (= copy from state)
        with (distribute $ frust) $ \ptr ->
              glUniformMatrix4fv proj 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))
        checkError "foo"

        --set up camera
        let ! cam = getCam (camX,camY) zDist xa ya
        with (distribute $ cam) $ \ptr ->
              glUniformMatrix4fv vmat 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))
        checkError "foo"
              
        --set up normal--Mat transpose((model*camera)^-1)
        let normal = (case inv33 ((fmap (^._xyz) cam) ^. _xyz) of
                                             (Just a) -> a
                                             Nothing  -> eye3) :: M33 CFloat
            nmap = (collect (fmap id) normal) :: M33 CFloat --transpose...
        
        with (distribute $ nmap) $ \ptr ->
              glUniformMatrix3fv nmat 1 0 (castPtr (ptr :: Ptr (M33 CFloat)))

        checkError "nmat"

        GL.bindBuffer GL.ArrayBuffer GL.$= Just map'
        GL.vertexAttribPointer ci GL.$= fgColorIndex
        GL.vertexAttribArray ci   GL.$= GL.Enabled
        GL.vertexAttribPointer ni GL.$= fgNormalIndex
        GL.vertexAttribArray ni   GL.$= GL.Enabled
        GL.vertexAttribPointer vi GL.$= fgVertexIndex
        GL.vertexAttribArray vi   GL.$= GL.Enabled
        checkError "beforeDraw"

        GL.drawArrays GL.Triangles 0 numVert
        checkError "draw"


-- Main game loop

run :: Pioneers ()
run = do
    win <- asks envWindow

    -- draw Scene
    draw
    liftIO $ do
        glSwapWindow win
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
    (kxrot, kyrot) <- fmap ((join (***)) fromIntegral) getArrowMovement
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
                                Closing -> modify $ \s -> s {
                                                        stateWinClose = True
                                                }
                                _ -> return ()
                Keyboard movement _ repeat key -> --up/down window(ignored) true/false actualKey
                        -- need modifiers? use "keyModifiers key" to get them
                        case keyScancode key of
                                Escape   -> modify $ \s -> s {
                                                        stateWinClose = True
                                                }
                                SDL.Left  -> modify $ \s -> s {
                                                stateArrowsPressed = (stateArrowsPressed s) {
                                                        arrowLeft = movement == KeyDown
                                                        }
                                                }
                                SDL.Right -> modify $ \s -> s {
                                                stateArrowsPressed = (stateArrowsPressed s) {
                                                        arrowRight = movement == KeyDown
                                                        }
                                                }
                                SDL.Up    -> modify $ \s -> s {
                                                stateArrowsPressed = (stateArrowsPressed s) {
                                                        arrowUp = movement == KeyDown
                                                        }
                                                }
                                SDL.Down  -> modify $ \s -> s {
                                                stateArrowsPressed = (stateArrowsPressed s) {
                                                        arrowDown = movement == KeyDown
                                                        }
                                                }
                                _ -> return ()
                MouseMotion _ id st (Position x y) xrel yrel -> do
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
                MouseButton _ id button state (Position x y) ->
                        case button of
                                LeftButton -> do
                                      let pressed = state == Pressed
                                      modify $ \s -> s
                                        { stateMouseDown = pressed
                                        }
                                      unless pressed $
                                        modify $ \s -> s
                                          { stateDragging = False
                                          }
                                _ -> return ()
                MouseWheel _ id hscroll vscroll -> do
                          env <- ask
                          modify $ \s -> s
                            { stateZDist =
                                let zDist' = stateZDist s + realToFrac (negate $ vscroll)
                                in curb (envZDistClosest env) (envZDistFarthest env) zDist'
                            }
                Quit -> modify $ \s -> s {stateWinClose = True}
                -- there is more (joystic, touchInterface, ...), but currently ignored
                _ -> return ()
        liftIO $ putStrLn $ unwords ["Processing Event:",(show e)]