{-# LANGUAGE BangPatterns #-}
module Main where

-- Monad-foo
import           Control.Applicative
import           Control.Monad                        (unless, void, when)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
-- data consistency
import           Control.Concurrent.STM               (TQueue, atomically,
                                                       newTQueueIO,
                                                       tryReadTQueue,
                                                       writeTQueue)
import           Control.Monad.RWS.Strict             (RWST, ask, asks,
                                                       evalRWST, get, liftIO,
                                                       modify, put)
-- FFI
import           Foreign                              (Ptr, castPtr, with)
import           Foreign.C                            (CFloat)

-- Math
import           Control.Lens                         (transposeOf, (^.))
import           Linear                               as L

-- GUI
import           Graphics.UI.SDL as SDL

-- Render
import qualified Graphics.Rendering.OpenGL.GL         as GL
import           Graphics.Rendering.OpenGL.Raw.Core31

-- Our modules
import           Map.Map
import           Render.Misc                          (checkError,
                                                       createFrustum, getCam,
                                                       lookAt, up)
import           Render.Render                        (initRendering,
                                                       initShader)

--Static Read-Only-State
data Env = Env
    { envEventsChan    :: TQueue Event
    , envWindow        :: !Window
    , envZDistClosest  :: !Double
    , envZDistFarthest :: !Double
    }

--Mutable State
data State = State
    { stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    , stateWinClose        :: !Bool
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
        SDL.withInit [InitEverything] $ do --also: InitNoParachute -> faster, without parachute!
        window <- SDL.createWindow "Pioneers" (Position 100 100) (Size 1024 768) [WindowOpengl     -- we want openGL
                                                                             ,WindowShown      -- window should be visible
                                                                             ,WindowResizable  -- and resizable 
                                                                             ,WindowInputFocus -- focused (=> active)
                                                                             ,WindowMouseFocus -- Mouse into it
                                                                             --,WindowInputGrabbed-- never let go of input (KB/Mouse)
                                                                             ]

        (Size fbWidth fbHeight) <- glGetDrawableSize window
        initRendering
        --generate map vertices
        (mapBuffer, vert) <- getMapBufferObject
        (ci, ni, vi, pri, vii, mi, nmi) <- initShader
        eventQueue <- newTQueueIO :: IO (TQueue Event)

        let zDistClosest  = 10
            zDistFarthest = zDistClosest + 20
            fov           = 90  --field of view
            near          = 1   --near plane
            far           = 100 --far plane
            ratio         = fromIntegral fbWidth / fromIntegral fbHeight
            frust         = createFrustum fov near far ratio
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
              }
        void $ evalRWST (adjustWindow >> run) env state

        destroyWindow window

-- Main game loop

run :: Pioneers ()
run = do
    win <- asks envWindow
    events <- asks envEventsChan

    -- draw Scene
    --draw
    liftIO $ do
        glSwapWindow win
        submitEvents events
    -- getEvents & process
    processEvents

    -- update State

    state <- get
    -- change in camera-angle
    {- if stateDragging state
      then do
          let sodx  = stateDragStartX      state
              sody  = stateDragStartY      state
              sodxa = stateDragStartXAngle state
              sodya = stateDragStartYAngle state
          (x, y) <- liftIO $ GLFW.getCursorPos win
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
--          liftIO $ putStrLn $ unwords $ map show $ [newXAngle, newYAngle]
      else do
          (jxrot, jyrot) <- liftIO $ getJoystickDirections GLFW.Joystick'1
          put $ state
            { stateXAngle = stateXAngle state + (2 * jxrot)
            , stateYAngle = stateYAngle state + (2 * jyrot)
            }

    -- get cursor-keys - if pressed
    --TODO: Add sin/cos from stateYAngle
    (kxrot, kyrot) <- liftIO $ getCursorKeyDirections win
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
    -}
    {-
    --modify the state with all that happened in mt time.
    mt <- liftIO GLFW.getTime
    modify $ \s -> s
      {
      }
    -}


    unless (stateWinClose state) run

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


-- | Writes all Events atomically to global Queue for further processing.
submitEvents :: TQueue Event -> IO ()
submitEvents q = do
        event <- pollEvent
        case event of 
                Nothing -> return ()
                Just e -> do
                                atomically $ writeTQueue q e
                                submitEvents q

processEvents :: Pioneers ()
processEvents = do
                return ()