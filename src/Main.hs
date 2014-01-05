{-# LANGUAGE BangPatterns #-}
module Main (main) where

--------------------------------------------------------------------------------

import           Control.Concurrent.STM               (TQueue, atomically,
                                                       newTQueueIO,
                                                       tryReadTQueue,
                                                       writeTQueue)
import           Control.Monad                        (unless, void, when)
import           Control.Monad.RWS.Strict             (RWST, ask, asks,
                                                       evalRWST, get, liftIO,
                                                       modify, put)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import           Data.Distributive                    (distribute)
import           Data.List                            (intercalate)
import           Data.Maybe                           (catMaybes)
import           Foreign                              (Ptr, castPtr, with)
import           Foreign.C                            (CFloat)
import           Linear                               as L
import           Text.PrettyPrint

import qualified Graphics.Rendering.OpenGL.GL         as GL
import           Graphics.Rendering.OpenGL.Raw.Core31
import qualified Graphics.UI.GLFW                     as GLFW

import           Map.Map
import           Render.Misc                          (checkError,
                                                       createFrustum, getCam,
                                                       lookAt, up)
import           Render.Render                        (initRendering,
                                                       initShader)

--------------------------------------------------------------------------------

--Static Read-Only-State
data Env = Env
    { envEventsChan    :: TQueue Event
    , envWindow        :: !GLFW.Window
    , envZDistClosest  :: !Double
    , envZDistFarthest :: !Double
    }

--Mutable State
data State = State
    { stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
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
    --- the map
    , stateMap             :: !GL.BufferObject
    , mapVert              :: !GL.NumArrayIndices
    }

type Pioneer = RWST Env () State IO

--------------------------------------------------------------------------------

data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !GLFW.FocusState
  | EventWindowIconify   !GLFW.Window !GLFW.IconifyState
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  deriving Show

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let width  = 640
        height = 480

    eventsChan <- newTQueueIO :: IO (TQueue Event)

    withWindow width height "Pioneers" $ \win -> do
        GLFW.setErrorCallback               $ Just $ errorCallback           eventsChan
        GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       eventsChan
        GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      eventsChan
        GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     eventsChan
        GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   eventsChan
        GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     eventsChan
        GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   eventsChan
        GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventsChan
        GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     eventsChan
        GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       eventsChan
        GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     eventsChan
        GLFW.setScrollCallback          win $ Just $ scrollCallback          eventsChan
        GLFW.setKeyCallback             win $ Just $ keyCallback             eventsChan
        GLFW.setCharCallback            win $ Just $ charCallback            eventsChan

        GLFW.swapInterval 1

        (fbWidth, fbHeight) <- GLFW.getFramebufferSize win

        initRendering
        --generate map vertices
        (mapBuffer, vert) <- getMapBufferObject
        (ci, ni, vi, pri, vii, mi) <- initShader

        let zDistClosest  = 10
            zDistFarthest = zDistClosest + 20
            fov           = 90  --field of view
            near          = 1   --near plane
            far           = 100 --far plane
            ratio         = fromIntegral fbWidth / fromIntegral fbHeight
            frust         = createFrustum fov near far ratio
            env = Env
              { envEventsChan    = eventsChan
              , envWindow        = win
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
              , stateMap             = mapBuffer
              , mapVert              = vert
              , stateFrustum         = frust
              }
        runDemo env state

    putStrLn "ended!"

--------------------------------------------------------------------------------

-- GLFW-b is made to be very close to the C API, so creating a window is pretty
-- clunky by Haskell standards. A higher-level API would have some function
-- like withWindow.

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

--------------------------------------------------------------------------------

-- Each callback does just one thing: write an appropriate Event to the events
-- TQueue.

errorCallback           :: TQueue Event -> GLFW.Error -> String                                                            -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> GLFW.FocusState                                                  -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> GLFW.IconifyState                                                -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                             -> IO ()

errorCallback           tc e s            = atomically $ writeTQueue tc $ EventError           e s
windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = atomically $ writeTQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = atomically $ writeTQueue tc $ EventChar            win c

--------------------------------------------------------------------------------

runDemo :: Env -> State -> IO ()
runDemo env state = void $ evalRWST (adjustWindow >> run) env state

run :: Pioneer ()
run = do
    win <- asks envWindow

    -- draw Scene
    draw
    liftIO $ do
        GLFW.swapBuffers win
        GLFW.pollEvents
    -- getEvents & process
    processEvents

    -- update State

    state <- get
    -- change in camera-angle
    if stateDragging state
      then do
          let sodx  = stateDragStartX      state
              sody  = stateDragStartY      state
              sodxa = stateDragStartXAngle state
              sodya = stateDragStartYAngle state
          (x, y) <- liftIO $ GLFW.getCursorPos win
          let myrot = (x - sodx) / 2
              mxrot = (y - sody) / 2
              newXAngle  = curb 0 (0.45*pi) newXAngle'
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
        
    {-
    --modify the state with all that happened in mt time.
    mt <- liftIO GLFW.getTime
    modify $ \s -> s
      {
      }
    -}

    q <- liftIO $ GLFW.windowShouldClose win
    unless q run

processEvents :: Pioneer ()
processEvents = do
    tc <- asks envEventsChan
    me <- liftIO $ atomically $ tryReadTQueue tc
    case me of
      Just e -> do
          processEvent e
          processEvents
      Nothing -> return ()

processEvent :: Event -> Pioneer ()
processEvent ev =
    case ev of
      (EventError e s) -> do
          printEvent "error" [show e, show s]
          win <- asks envWindow
          liftIO $ GLFW.setWindowShouldClose win True

      (EventWindowPos _ x y) ->
          printEvent "window pos" [show x, show y]

      (EventWindowSize _ width height) ->
          printEvent "window size" [show width, show height]

      (EventWindowClose _) ->
          printEvent "window close" []

      (EventWindowRefresh _) ->
          printEvent "window refresh" []

      (EventWindowFocus _ fs) ->
          printEvent "window focus" [show fs]

      (EventWindowIconify _ is) ->
          printEvent "window iconify" [show is]

      (EventFramebufferSize _ width height) -> do
          printEvent "framebuffer size" [show width, show height]
          modify $ \s -> s
            { stateWindowWidth  = width
            , stateWindowHeight = height
            }
          adjustWindow

      (EventMouseButton _ mb mbs mk) -> do
          printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
          when (mb == GLFW.MouseButton'1) $ do
              let pressed = mbs == GLFW.MouseButtonState'Pressed
              modify $ \s -> s
                { stateMouseDown = pressed
                }
              unless pressed $
                modify $ \s -> s
                  { stateDragging = False
                  }

      (EventCursorPos _ x y) -> do
          {-let x' = round x :: Int
              y' = round y :: Int
          printEvent "cursor pos" [show x', show y']-}
          state <- get
          when (stateMouseDown state && not (stateDragging state)) $
            put $ state
              { stateDragging        = True
              , stateDragStartX      = x
              , stateDragStartY      = y
              , stateDragStartXAngle = stateXAngle state
              , stateDragStartYAngle = stateYAngle state
              }

      (EventCursorEnter _ cs) ->
          printEvent "cursor enter" [show cs]

      (EventScroll _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          printEvent "scroll" [show x', show y']
          env <- ask
          modify $ \s -> s
            { stateZDist =
                let zDist' = stateZDist s + realToFrac (negate $ y)
                in curb (envZDistClosest env) (envZDistFarthest env) zDist'
            }
          adjustWindow

      (EventKey win k scancode ks mk) -> do
          when (ks == GLFW.KeyState'Pressed) $ do
              -- Q, Esc: exit
              when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
                liftIO $ GLFW.setWindowShouldClose win True
              -- i: print GLFW information
              when (k == GLFW.Key'I) $
                liftIO $ printInformation win
          unless (elem k [GLFW.Key'Up
                         ,GLFW.Key'Down
                         ,GLFW.Key'Left
                         ,GLFW.Key'Right
                         ]) $ do
                printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]

      (EventChar _ c) ->
          printEvent "char" [show c]

adjustWindow :: Pioneer ()
adjustWindow = do
    state <- get
    let fbWidth  = stateWindowWidth  state
        fbHeight = stateWindowHeight state
        fov           = 90  --field of view
        near          = 1   --near plane
        far           = 100 --far plane
        ratio         = fromIntegral fbWidth / fromIntegral fbHeight
        frust         = createFrustum fov near far ratio
    put $ state {
        stateFrustum = frust
    }

draw :: Pioneer ()
draw = do
    env   <- ask
    state <- get
    let xa       = stateXAngle          state
        ya       = stateYAngle          state
        (GL.UniformLocation proj)  = shdrProjMatIndex state
        (GL.UniformLocation vmat)  = shdrViewMatIndex state
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
        GL.clearColor GL.$= GL.Color4 0.5 0.1 1 1
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        --set up projection (= copy from state)
        with (distribute $ frust) $ \ptr ->
              glUniformMatrix4fv proj 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))

        --set up camera
        let ! cam = getCam (camX,camY) zDist xa ya
        with (distribute $ cam) $ \ptr ->
              glUniformMatrix4fv vmat 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))

        GL.bindBuffer GL.ArrayBuffer GL.$= Just map'
        GL.vertexAttribPointer ci GL.$= fgColorIndex
        GL.vertexAttribArray ci   GL.$= GL.Enabled
        GL.vertexAttribPointer ni GL.$= fgNormalIndex
        GL.vertexAttribArray ni   GL.$= GL.Enabled
        GL.vertexAttribPointer vi GL.$= fgVertexIndex
        GL.vertexAttribArray vi   GL.$= GL.Enabled

        GL.drawArrays GL.Triangles 0 numVert
        checkError "draw"

getCursorKeyDirections :: GLFW.Window -> IO (Double, Double)
getCursorKeyDirections win = do
    y0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Up
    y1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Down
    x0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Left
    x1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Right
    let x0n = if x0 then (-1) else 0
        x1n = if x1 then   1  else 0
        y0n = if y0 then (-1) else 0
        y1n = if y1 then   1  else 0
    return (x0n + x1n, y0n + y1n)

getJoystickDirections :: GLFW.Joystick -> IO (Double, Double)
getJoystickDirections js = do
    maxes <- GLFW.getJoystickAxes js
    return $ case maxes of
      (Just (x:y:_)) -> (-y, x)
      _              -> ( 0, 0)

isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed   = True
isPress GLFW.KeyState'Repeating = True
isPress _                       = False

--------------------------------------------------------------------------------

printInformation :: GLFW.Window -> IO ()
printInformation win = do
    version       <- GLFW.getVersion
    versionString <- GLFW.getVersionString
    monitorInfos  <- runMaybeT getMonitorInfos
    joystickNames <- getJoystickNames
    clientAPI     <- GLFW.getWindowClientAPI              win
    cv0           <- GLFW.getWindowContextVersionMajor    win
    cv1           <- GLFW.getWindowContextVersionMinor    win
    cv2           <- GLFW.getWindowContextVersionRevision win
    robustness    <- GLFW.getWindowContextRobustness      win
    forwardCompat <- GLFW.getWindowOpenGLForwardCompat    win
    debug         <- GLFW.getWindowOpenGLDebugContext     win
    profile       <- GLFW.getWindowOpenGLProfile          win

    putStrLn $ render $
      nest 4 (
        text "------------------------------------------------------------" $+$
        text "GLFW C library:" $+$
        nest 4 (
          text "Version:"        <+> renderVersion version $+$
          text "Version string:" <+> renderVersionString versionString
        ) $+$
        text "Monitors:" $+$
        nest 4 (
          renderMonitorInfos monitorInfos
        ) $+$
        text "Joysticks:" $+$
        nest 4 (
          renderJoystickNames joystickNames
        ) $+$
        text "OpenGL context:" $+$
        nest 4 (
          text "Client API:"            <+> renderClientAPI clientAPI $+$
          text "Version:"               <+> renderContextVersion cv0 cv1 cv2 $+$
          text "Robustness:"            <+> renderContextRobustness robustness $+$
          text "Forward compatibility:" <+> renderForwardCompat forwardCompat $+$
          text "Debug:"                 <+> renderDebug debug $+$
          text "Profile:"               <+> renderProfile profile
        ) $+$
        text "------------------------------------------------------------"
      )
  where
    renderVersion (GLFW.Version v0 v1 v2) =
        text $ intercalate "." $ map show [v0, v1, v2]

    renderVersionString =
        text . show

    renderMonitorInfos =
        maybe (text "(error)") (vcat . map renderMonitorInfo)

    renderMonitorInfo (name, (x,y), (w,h), vms) =
        text (show name) $+$
        nest 4 (
          location <+> size $+$
          fsep (map renderVideoMode vms)
        )
      where
        location = int x <> text "," <> int y
        size     = int w <> text "x" <> int h <> text "mm"

    renderVideoMode (GLFW.VideoMode w h r g b rr) =
        brackets $ res <+> rgb <+> hz
      where
        res = int w <> text "x" <> int h
        rgb = int r <> text "x" <> int g <> text "x" <> int b
        hz  = int rr <> text "Hz"

    renderJoystickNames pairs =
        vcat $ map (\(js, name) -> text (show js) <+> text (show name)) pairs

    renderContextVersion v0 v1 v2 =
        hcat [int v0, text ".", int v1, text ".", int v2]

    renderClientAPI         = text . show
    renderContextRobustness = text . show
    renderForwardCompat     = text . show
    renderDebug             = text . show
    renderProfile           = text . show

type MonitorInfo = (String, (Int,Int), (Int,Int), [GLFW.VideoMode])

getMonitorInfos :: MaybeT IO [MonitorInfo]
getMonitorInfos =
    getMonitors >>= mapM getMonitorInfo
  where
    getMonitors :: MaybeT IO [GLFW.Monitor]
    getMonitors = MaybeT GLFW.getMonitors

    getMonitorInfo :: GLFW.Monitor -> MaybeT IO MonitorInfo
    getMonitorInfo mon = do
        name <- getMonitorName mon
        vms  <- getVideoModes mon
        MaybeT $ do
            pos  <- liftIO $ GLFW.getMonitorPos mon
            size <- liftIO $ GLFW.getMonitorPhysicalSize mon
            return $ Just (name, pos, size, vms)

    getMonitorName :: GLFW.Monitor -> MaybeT IO String
    getMonitorName mon = MaybeT $ GLFW.getMonitorName mon

    getVideoModes :: GLFW.Monitor -> MaybeT IO [GLFW.VideoMode]
    getVideoModes mon = MaybeT $ GLFW.getVideoModes mon

getJoystickNames :: IO [(GLFW.Joystick, String)]
getJoystickNames =
    catMaybes `fmap` mapM getJoystick joysticks
  where
    getJoystick js =
        fmap (maybe Nothing (\name -> Just (js, name)))
             (GLFW.getJoystickName js)

--------------------------------------------------------------------------------

printEvent :: String -> [String] -> Pioneer ()
printEvent cbname fields =
    liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk =
    "[mod keys: " ++ keys ++ "]"
  where
    keys = if null xs then "none" else unwords xs
    xs = catMaybes ys
    ys = [ if GLFW.modifierKeysShift   mk then Just "shift"   else Nothing
         , if GLFW.modifierKeysControl mk then Just "control" else Nothing
         , if GLFW.modifierKeysAlt     mk then Just "alt"     else Nothing
         , if GLFW.modifierKeysSuper   mk then Just "super"   else Nothing
         ]

curb :: Ord a => a -> a -> a -> a
curb l h x
  | x < l     = l
  | x > h     = h
  | otherwise = x

--------------------------------------------------------------------------------

joysticks :: [GLFW.Joystick]
joysticks =
  [ GLFW.Joystick'1
  , GLFW.Joystick'2
  , GLFW.Joystick'3
  , GLFW.Joystick'4
  , GLFW.Joystick'5
  , GLFW.Joystick'6
  , GLFW.Joystick'7
  , GLFW.Joystick'8
  , GLFW.Joystick'9
  , GLFW.Joystick'10
  , GLFW.Joystick'11
  , GLFW.Joystick'12
  , GLFW.Joystick'13
  , GLFW.Joystick'14
  , GLFW.Joystick'15
  , GLFW.Joystick'16
  ]
