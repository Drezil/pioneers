{-# LANGUAGE BangPatterns, DoAndIfThenElse #-}
module Main where

import Data.Int (Int8)
import Graphics.Rendering.OpenGL.GL.Texturing.Specification (TextureSize2D)
import Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable (PixelInternalFormat(..))
import Graphics.Rendering.OpenGL.GL.Texturing.Specification (texImage2D)
import Graphics.Rendering.OpenGL.GL.Texturing.Parameters (textureFilter)
import Graphics.Rendering.OpenGL.GL.Texturing.Specification (TextureTarget2D(Texture2D))
import Graphics.Rendering.OpenGL.GL.Texturing.Objects (textureBinding)
import Graphics.Rendering.OpenGL.GL.Texturing.Parameters (TextureFilter(..))

-- Monad-foo and higher functional stuff
import           Control.Monad                        (unless, void, when, join, liftM)
import           Control.Arrow                        ((***))

-- data consistency/conversion
import           Control.Concurrent                   (threadDelay)
import           Control.Concurrent.STM               (TQueue,
                                                       newTQueueIO)

import           Control.Monad.RWS.Strict             (RWST, ask, asks,
                                                       evalRWST, get, liftIO,
                                                       modify, put)
import           Data.Distributive                    (distribute, collect)

-- FFI
import           Foreign                              (Ptr, castPtr, with, sizeOf)
import           Foreign.C                            (CFloat)
import           Foreign.C.Types                      (CInt)
import           Foreign.Marshal.Array                (pokeArray)
import           Foreign.Marshal.Alloc                (allocaBytes)
import           Data.Word                            (Word8)

-- Math
import           Control.Lens                         ((^.), (.~), (%~))
import qualified Linear                               as L

-- GUI
import           Graphics.UI.SDL                      as SDL
--import           Graphics.UI.SDL.TTF                  as TTF
--import           Graphics.UI.SDL.TTF.Types

-- Render
import qualified Graphics.Rendering.OpenGL.GL         as GL
import           Graphics.Rendering.OpenGL.Raw.Core31
import           Data.Time                            (getCurrentTime, diffUTCTime)
import           Graphics.GLUtil.BufferObjects        (offset0)

import Graphics.Rendering.OpenGL.Raw.ARB.TessellationShader
-- Our modules
import           Map.Graphics
import           Render.Misc                          (checkError,
                                                       createFrustum, getCam,
                                                       curb, tryWithTexture,
                                                       genColorData)
import           Render.Render                        (initRendering,
                                                       initMapShader,
                                                       initHud)
import           UI.Callbacks
import           UI.GUIOverlay
import           Types

--import           ThirdParty.Flippers

import qualified Debug.Trace                          as D (trace)

--------------------------------------------------------------------------------
main :: IO ()
main = do
     SDL.withInit [InitVideo, InitAudio, InitEvents, InitTimer] $ do --also: InitNoParachute -> faster, without parachute!
      SDL.withWindow "Pioneers" (SDL.Position 100 100) (Size 1024 600) [WindowOpengl     -- we want openGL
                                                                             ,WindowShown      -- window should be visible
                                                                             ,WindowResizable  -- and resizable 
                                                                             ,WindowInputFocus -- focused (=> active)
                                                                             ,WindowMouseFocus -- Mouse into it
                                                                             --,WindowInputGrabbed-- never let go of input (KB/Mouse)
                                                                             ] $ \window -> do
       withOpenGL window $ do
        
        --Create Renderbuffer & Framebuffer
        -- We will render to this buffer to copy the result into textures
        renderBuffer <- GL.genObjectName
        frameBuffer <- GL.genObjectName
        GL.bindFramebuffer GL.Framebuffer GL.$= frameBuffer
        GL.bindRenderbuffer GL.Renderbuffer GL.$= renderBuffer
        
        (Size fbWidth fbHeight) <- glGetDrawableSize window
        initRendering
        --generate map vertices
        (mapBuffer, vert) <- getMapBufferObject
        (mapprog, ci, ni, vi, pri, vii, mi, nmi, tli, tlo, mapTex) <- initMapShader
        print window
        eventQueue <- newTQueueIO :: IO (TQueue Event)
        putStrLn "foo"
        now <- getCurrentTime
        putStrLn "foo"
        --font <- TTF.openFont "fonts/ttf-04B_03B_/04B_03B_.TTF" 10
        --TTF.setFontStyle font TTFNormal
        --TTF.setFontHinting font TTFHNormal

        glHud <- initHud
        let zDistClosest  = 1
            zDistFarthest = zDistClosest + 50
            --TODO: Move near/far/fov to state for runtime-changability & central storage
            fov           = 90  --field of view
            near          = 1   --near plane
            far           = 500 --far plane
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
                , _mapProgram           = mapprog
                , _mapTexture           = mapTex
                }
            env = Env
              { _eventsChan      = eventQueue
              , _windowObject    = window
              , _zDistClosest    = zDistClosest
              , _zDistFarthest   = zDistFarthest
              --, _renderer        = renderer 
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
                                       { Types.__x    = 25
                                       , Types.__y    = 25
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
                                         { Types.__x  = 5
                                         , Types.__y  = 5
                                         }
                        }
              , _keyboard            = KeyboardState
                        { _arrowsPressed       = aks
                        }
              , _gl                  = GLState
                        { _glMap               = glMap
                        , _glHud               = glHud
                        , _glRenderbuffer      = renderBuffer
                        , _glFramebuffer       = frameBuffer
                        }
              , _game                = GameState
                        {
                        }
              , _ui                  = UIState
                        { _uiHasChanged        = True
                        }
              }

        putStrLn "init done."
        void $ evalRWST (adjustWindow >> run) env state
        
        --SDL.glDeleteContext mainGlContext
        --SDL.destroyRenderer renderer
        --destroyWindow window

-- Render-Pipeline

draw :: Pioneers ()
draw = do
    state <- get
    env <- ask
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
        camX     = state ^. camera.camPosition._x
        camY     = state ^. camera.camPosition._y
        zDist'   = state ^. camera.zDist
        tessFac  = state ^. gl.glMap.stateTessellationFactor
        window   = env ^. windowObject
        rb       = state ^. gl.glRenderbuffer
    when (state ^. ui . uiHasChanged) prepareGUI
    liftIO $ do
        --bind renderbuffer and set sample 0 as target
        --GL.bindRenderbuffer GL.Renderbuffer GL.$= rb
        --GL.bindFramebuffer  GL.Framebuffer  GL.$= GL.defaultFramebufferObject 
        --checkError "bind renderbuffer"

        --checkError "clear renderbuffer"
        {-GL.framebufferRenderbuffer
                GL.Framebuffer                  --framebuffer
                (GL.ColorAttachment 1)          --sample 1
                GL.Renderbuffer                 --const
                rb                              --buffer
        checkError "setup renderbuffer"-}

        -- draw map
        --(vi,GL.UniformLocation proj) <- initShader
        
        GL.bindFramebuffer GL.Framebuffer GL.$= (state ^. gl.glFramebuffer)
        GL.bindRenderbuffer GL.Renderbuffer GL.$= (state ^. gl.glRenderbuffer)
        GL.framebufferRenderbuffer
                GL.Framebuffer
                GL.DepthAttachment
                GL.Renderbuffer
                (state ^. gl.glRenderbuffer)
        textureBinding GL.Texture2D GL.$= Just (state ^. gl.glMap.mapTexture)
        
        GL.framebufferTexture2D
                GL.Framebuffer
                (GL.ColorAttachment 0)
                GL.Texture2D
                (state ^. gl.glMap.mapTexture)
                0
        
        -- Render to FrameBufferObject
        GL.drawBuffers GL.$= [GL.FBOColorAttachment 0]
        checkError "setup Render-Target"

        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        checkError "clear buffer"


        GL.currentProgram GL.$= Just (state ^. gl.glMap.mapProgram)

        checkError "setting up buffer"
        --set up projection (= copy from state)
        with (distribute frust) $ \ptr ->
              glUniformMatrix4fv proj 1 0 (castPtr (ptr :: Ptr (L.M44 CFloat)))
        checkError "copy projection"

        --set up camera
        let ! cam = getCam (camX,camY) zDist' xa ya
        with (distribute cam) $ \ptr ->
              glUniformMatrix4fv vmat 1 0 (castPtr (ptr :: Ptr (L.M44 CFloat)))
        checkError "copy cam"

        --set up normal--Mat transpose((model*camera)^-1)
        let normal = (case L.inv33 (fmap (^. L._xyz) cam ^. L._xyz) of
                                             (Just a) -> a
                                             Nothing  -> L.eye3) :: L.M33 CFloat
            nmap = collect id normal :: L.M33 CFloat --transpose...

        with (distribute nmap) $ \ptr ->
              glUniformMatrix3fv nmat 1 0 (castPtr (ptr :: Ptr (L.M33 CFloat)))

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
        checkError "draw map"

        -- set sample 1 as target in renderbuffer
        {-GL.framebufferRenderbuffer
                GL.DrawFramebuffer              --write-only
                (GL.ColorAttachment 1)          --sample 1
                GL.Renderbuffer                 --const
                rb                              --buffer-}

        -- Render to BackBuffer (=Screen)
        GL.bindFramebuffer GL.Framebuffer GL.$= GL.defaultFramebufferObject
        GL.drawBuffer GL.$= GL.BackBuffers
        -- Drawing HUD
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        checkError "clear buffer"

        let hud    = state ^. gl.glHud
            stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
            vad    = GL.VertexArrayDescriptor 2 GL.Float stride offset0
        GL.currentProgram GL.$= Just (hud ^. hudProgram)

        GL.activeTexture  GL.$= GL.TextureUnit 0
        textureBinding GL.Texture2D GL.$= Just (hud ^. hudTexture)
        GL.uniform (hud ^. hudTexIndex) GL.$= GL.Index1 (0::GL.GLint)

        GL.activeTexture  GL.$= GL.TextureUnit 1
        textureBinding GL.Texture2D GL.$= Just (state ^. gl.glMap.mapTexture)
        GL.uniform (hud ^. hudBackIndex) GL.$= GL.Index1 (1::GL.GLint)
        
        GL.bindBuffer GL.ArrayBuffer GL.$= Just (hud ^. hudVBO)
        GL.vertexAttribPointer (hud ^. hudVertexIndex) GL.$= (GL.ToFloat, vad)
        GL.vertexAttribArray   (hud ^. hudVertexIndex) GL.$= GL.Enabled
        
        GL.bindBuffer GL.ElementArrayBuffer GL.$= Just (hud ^. hudEBO)
        GL.drawElements GL.TriangleStrip 4 GL.UnsignedInt offset0


        {-let winRenderer = env ^. renderer
        tryWithTexture
                (state ^. gl.hudTexture)                          --maybe tex
                (\tex -> renderCopy winRenderer tex Nothing Nothing) --function with "hole"
                                                       --Nothing == whole source-tex, whole dest-tex
                (return ())                                       --fail-case-}

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
    modify $ (camera.camPosition._x %~ modx)
           . (camera.camPosition._y %~ mody)

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
    env <- ask
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
                       maptexid = state ^. gl.glMap.mapTexture
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
    me <- liftIO pollEvent
    case me of
      Just e -> do
          processEvent e
          processEvents
      Nothing -> return ()

processEvent :: Event -> Pioneers ()
processEvent e = do
        env <- ask
        case eventData e of
            Window _ winEvent ->
                case winEvent of
                    Closing ->
                            modify $ window.shouldClose .~ True
                    Resized {windowResizedTo=size} -> do
                            modify $ (window . width .~ sizeWidth size)
                                   . (window . height .~ sizeHeight size)
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
                    SDL.R    ->
                        liftIO $ do
                                r <- getRenderer $ env ^. windowObject
                                putStrLn $ unwords ["Renderer: ",show r]
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
                    
                modify $ (mouse.mousePosition. Types._x .~ (fromIntegral x))
                       . (mouse.mousePosition. Types._y .~ (fromIntegral y))
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
