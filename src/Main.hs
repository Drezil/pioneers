{-# LANGUAGE BangPatterns, DoAndIfThenElse #-}
module Main where

import Graphics.Rendering.OpenGL.GL.Texturing.Specification (texImage2D,TextureTarget2D(Texture2D))
import Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable (PixelInternalFormat(..))
import Graphics.Rendering.OpenGL.GL.Texturing.Objects (textureBinding)
import Graphics.Rendering.OpenGL.GL.Texturing.Parameters (TextureFilter(..),textureFilter)

-- Monad-foo and higher functional stuff
import           Control.Monad                        (unless, when, join)
import           Control.Arrow                        ((***))

-- data consistency/conversion
import           Control.Concurrent                   (threadDelay)
import           Control.Concurrent.STM               (TQueue,
                                                       newTQueueIO)

import           Control.Monad.RWS.Strict             (ask, evalRWST, get, liftIO, modify)
import           Data.Functor                         ((<$>))
import           Data.Monoid                          (mappend)

-- FFI
import           Foreign.Marshal.Array                (pokeArray)
import           Foreign.Marshal.Alloc                (allocaBytes)

-- Math
import           Control.Lens                         ((^.), (.~), (%~))

-- GUI
import qualified Graphics.UI.SDL                      as SDL

-- Render
import qualified Graphics.Rendering.OpenGL.GL         as GL
import           Graphics.Rendering.OpenGL.Raw.Core31
import           Data.Time                            (getCurrentTime, diffUTCTime)

-- Our modules
import           Render.Misc                          (checkError, createFrustum, curb,
                                                       genColorData)
import           Render.Render                        (initRendering,
                                                       initMapShader,
                                                       initHud, render)
import           Render.Types
import           UI.Callbacks
import           Map.Graphics
import           Types
import           Importer.IQM.Parser
--import           Data.Attoparsec.Char8 (parseTest)
--import qualified Data.ByteString as B

-- import qualified Debug.Trace                          as D (trace)

--------------------------------------------------------------------------------

testParser :: String -> IO ()
testParser a = putStrLn . show  =<< parseIQM a
{-do
        f <- B.readFile a
        putStrLn "reading in:"
        putStrLn $ show f
        putStrLn "parsed:"
        parseTest parseIQM f-}

--------------------------------------------------------------------------------

main :: IO ()
main =
    SDL.withInit [SDL.InitVideo, SDL.InitAudio, SDL.InitEvents, SDL.InitTimer] $ --also: InitNoParachute -> faster, without parachute!
      SDL.withWindow "Pioneers" (SDL.Position 100 100) (SDL.Size 1024 600) [SDL.WindowOpengl     -- we want openGL
                                                                             ,SDL.WindowShown      -- window should be visible
                                                                             ,SDL.WindowResizable  -- and resizable
                                                                             ,SDL.WindowInputFocus -- focused (=> active)
                                                                             ,SDL.WindowMouseFocus -- Mouse into it
                                                                             --,WindowInputGrabbed-- never let go of input (KB/Mouse)
                                                                             ] $ \window' -> do
       SDL.withOpenGL window' $ do

        --Create Renderbuffer & Framebuffer
        -- We will render to this buffer to copy the result into textures
        renderBuffer <- GL.genObjectName
        frameBuffer <- GL.genObjectName
        GL.bindFramebuffer GL.Framebuffer GL.$= frameBuffer
        GL.bindRenderbuffer GL.Renderbuffer GL.$= renderBuffer

        (SDL.Size fbWidth fbHeight) <- SDL.glGetDrawableSize window'
        initRendering
        --generate map vertices
        glMap' <- initMapShader 4 =<< getMapBufferObject
        eventQueue <- newTQueueIO :: IO (TQueue SDL.Event)
        now <- getCurrentTime
        --font <- TTF.openFont "fonts/ttf-04B_03B_/04B_03B_.TTF" 10
        --TTF.setFontStyle font TTFNormal
        --TTF.setFontHinting font TTFHNormal

        glHud' <- initHud
        let zDistClosest'  = 1
            zDistFarthest' = zDistClosest' + 50
            --TODO: Move near/far/fov to state for runtime-changability & central storage
            fov           = 90  --field of view
            near          = 1   --near plane
            far           = 500 --far plane
            ratio         = fromIntegral fbWidth / fromIntegral fbHeight
            frust         = createFrustum fov near far ratio
            (guiMap, guiRoots) = createGUI
            aks = ArrowKeyState {
                  _up       = False
                , _down     = False
                , _left     = False
                , _right    = False
            }
            env = Env
              { _eventsChan      = eventQueue
              , _windowObject    = window'
              , _zDistClosest    = zDistClosest'
              , _zDistFarthest   = zDistFarthest'
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
                        , _camObject           = createFlatCam 25 25
                        }
              , _io                  = IOState
                        { _clock               = now
                        , _tessClockFactor     = 0
                        }
              , _mouse               = MouseState
                        { _isDown              = False
                        , _isDragging          = False
                        , _dragStartX          = 0
                        , _dragStartY          = 0
                        , _dragStartXAngle     = 0
                        , _dragStartYAngle     = 0
                        , _mousePosition       = Types.Position
                                         { Types.__x  = 5
                                         , Types.__y  = 5
                                         }
                        }
              , _keyboard            = KeyboardState
                        { _arrowsPressed       = aks
                        }
              , _gl                  = GLState
                        { _glMap               = glMap'
                        , _glHud               = glHud'
                        , _glRenderbuffer      = renderBuffer
                        , _glFramebuffer       = frameBuffer
                        }
              , _game                = GameState
                        {
                        }
              , _ui                  = UIState
                        { _uiHasChanged        = True
                        , _uiMap = guiMap
                        , _uiRoots = guiRoots
                        }
              }

        putStrLn "init done."
        uncurry mappend <$> evalRWST (adjustWindow >> run) env state
        putStrLn "shutdown complete."

        --SDL.glDeleteContext mainGlContext
        --SDL.destroyRenderer renderer
        --destroyWindow window

-- Main game loop

run :: Pioneers ()
run = do
    env <- ask

    -- draw Scene
    draw
    liftIO $ SDL.glSwapWindow (env ^. windowObject)
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
              x'    = state ^. mouse.mousePosition._x
              y'    = state ^. mouse.mousePosition._y
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
        mody y' = y' + 0.2 * kxrot * mults
                     - 0.2 * kyrot * multc
    modify $ camera.camObject %~ (\c -> moveBy c (\(x,y) -> (modx x,mody y)))

    {-
    --modify the state with all that happened in mt time.
    mt <- liftIO GLFW.getTime
    modify $ \s -> s
      {
      }
    -}

    (mt,tc,sleepAmount,frameTime) <- liftIO $ do
        let 	double = fromRational.toRational :: (Real a) => a -> Double
		targetFramerate = 60.0
		targetFrametime = 1.0/targetFramerate
		targetFrametimeμs = targetFrametime * 1000000.0
        now <- getCurrentTime
        let diff  = diffUTCTime now (state ^. io.clock) -- get time-diffs
            title = unwords ["Pioneers @ ",show ((round . double $ 1.0/diff)::Int),"fps"]
            ddiff = double diff
        SDL.setWindowTitle (env ^. windowObject) title
        let 	sleepAmount = floor ((targetFrametime - double diff)*1000000) :: Int -- get time until next frame in microseconds
                clockFactor = (state ^. io.tessClockFactor)
		tessChange
			| (clockFactor < (75*targetFrametime)) && (state ^. gl.glMap.stateTessellationFactor < 5) = ((+)1 :: Int -> Int) 
										-- > last 100 frames had > 25% leftover (on avg.)
			| (clockFactor > (110*targetFrametime)) && (state ^. gl.glMap.stateTessellationFactor > 1) = (flip (-) 1 :: Int -> Int)
									-- > last 100 frames had < 90% of target-fps
			| otherwise = ((+)0 :: Int -> Int)              -- 0ms > x > 10% -> keep settings
        when (sleepAmount > 0) $ threadDelay sleepAmount
	now' <- getCurrentTime
        return (now',tessChange,sleepAmount,ddiff)
    -- set state with new clock-time
    --liftIO $ putStrLn $ unwords ["clockFactor:",show (state ^. io.tessClockFactor),"\ttc:", show (tc (state ^. gl.glMap.stateTessellationFactor)),"\tsleep ",show frameTime,"ms"]
    modify $ (io.clock .~ mt)
           . (gl.glMap.stateTessellationFactor %~ tc)
           . (io.tessClockFactor %~ (((+) frameTime).((*) 0.99)))
    -- liftIO $ putStrLn $ concat $ ["TessFactor at: ",show (state ^. gl.glMap.stateTessellationFactor), " - slept for ",show sleepAmount, "μs."]
    shouldClose' <- return $ state ^. window.shouldClose
    unless shouldClose' run

draw :: Pioneers ()
draw = do
    state <- get
    when (state ^. ui . uiHasChanged) prepareGUI
    render

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
    rb <- liftIO $ do
                   -- bind ints to CInt for lateron.
                   let fbCWidth  = (fromInteger.toInteger) fbWidth
                       fbCHeight = (fromInteger.toInteger) fbHeight
                   -- free old renderbuffer & create new (reuse is NOT advised!)
                   GL.deleteObjectName (state ^. gl.glRenderbuffer)
                   renderBuffer <- GL.genObjectName
                   GL.bindRenderbuffer GL.Renderbuffer GL.$= renderBuffer
                   GL.renderbufferStorage
                       GL.Renderbuffer                         -- use the only available renderbuffer
                                                               -- - must be this constant.
                       GL.DepthComponent'                      -- 32-bit float-rgba-color
                       (GL.RenderbufferSize fbCWidth fbCHeight)  -- size of buffer


                   let hudtexid = state ^. gl.glHud.hudTexture
                       maptexid = state ^. gl.glMap.renderedMapTexture
                   allocaBytes (fbWidth*fbHeight*4) $ \ptr -> do
                                                               --default to ugly pink to see if
                                                               --somethings go wrong.
                        let imData = genColorData (fbWidth*fbHeight) [255,0,255,0]
                        --putStrLn $ show imData
                        pokeArray ptr imData
                        -- HUD
                        textureBinding Texture2D GL.$= Just hudtexid
                        textureFilter  Texture2D GL.$= ((Linear', Nothing), Linear')
                        texImage2D Texture2D GL.NoProxy 0 RGBA8 (GL.TextureSize2D fbCWidth fbCHeight) 0
                                                (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
                        -- MAP
                        textureBinding Texture2D GL.$= Just maptexid
                        textureFilter  Texture2D GL.$= ((Linear', Nothing), Linear')
                        texImage2D Texture2D GL.NoProxy 0 RGBA8 (GL.TextureSize2D fbCWidth fbCHeight) 0
                                                (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
                   checkError "setting up HUD-Tex"
                   return renderBuffer
    modify $ gl.glRenderbuffer .~ rb
    modify $ ui.uiHasChanged .~ True

processEvents :: Pioneers ()
processEvents = do
    me <- liftIO SDL.pollEvent
    case me of
      Just e -> do
          processEvent e
          processEvents
      Nothing -> return ()

processEvent :: SDL.Event -> Pioneers ()
processEvent e = do
    eventCallback e
    -- env <- ask
    case SDL.eventData e of
         SDL.Window _ winEvent -> -- windowID event
            case winEvent of
                SDL.Closing ->
                        modify $ window.shouldClose .~ True
                SDL.Resized {SDL.windowResizedTo=size} -> do
                        modify $ (window . width .~ SDL.sizeWidth size)
                               . (window . height .~ SDL.sizeHeight size)
                        adjustWindow
                SDL.SizeChanged ->
                        adjustWindow
                _ -> return ()
         _ -> return ()
