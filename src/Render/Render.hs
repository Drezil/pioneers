{-# LANGUAGE BangPatterns, InstanceSigs, ExistentialQuantification #-}
module Render.Render where

import qualified Data.ByteString                            as B
import           Foreign.Marshal.Array                      (withArray)
import           Foreign.Storable
import           Graphics.Rendering.OpenGL.GL
import           Graphics.Rendering.OpenGL.Raw.Core31
import           Graphics.Rendering.OpenGL.Raw.ARB.TessellationShader
import           Graphics.GLUtil.BufferObjects        (offset0)
import qualified Linear as L
import           Control.Lens                               ((^.))
import           Control.Monad.RWS.Strict             (liftIO)
import qualified Control.Monad.RWS.Strict as RWS      (get)
import           Data.Distributive                    (distribute, collect)
-- FFI
import           Foreign                              (Ptr, castPtr, with)
import           Foreign.C                            (CFloat)

import           Map.Graphics
import           Types
import           Render.Misc
import           Render.Types
import           Graphics.GLUtil.BufferObjects              (makeBuffer)
import		 Importer.IQM.Parser

mapVertexShaderFile :: String
mapVertexShaderFile = "shaders/map/vertex.shader"
mapTessControlShaderFile :: String
mapTessControlShaderFile = "shaders/map/tessControl.shader"
mapTessEvalShaderFile :: String
mapTessEvalShaderFile = "shaders/map/tessEval.shader"
mapFragmentShaderFile :: String
mapFragmentShaderFile = "shaders/map/fragment.shader"

objectVertexShaderFile :: String
objectVertexShaderFile = "shaders/objects/vertex.shader"
objectFragmentShaderFile :: String
objectFragmentShaderFile = "shaders/objects/fragment.shader"

uiVertexShaderFile :: String
uiVertexShaderFile = "shaders/ui/vertex.shader"
uiFragmentShaderFile :: String
uiFragmentShaderFile = "shaders/ui/fragment.shader"

initBuffer :: [GLfloat] -> IO BufferObject
initBuffer varray =
        let
                sizeOfVarray = length varray * sizeOfComponent
                sizeOfComponent = sizeOf (head varray)
        in do
           bufferObject <- genObjectName
           bindBuffer ArrayBuffer $= Just bufferObject
           withArray varray $ \buffer ->
              bufferData ArrayBuffer $= (fromIntegral sizeOfVarray, buffer, StaticDraw)
           checkError "initBuffer"
           return bufferObject

initMapShader ::
                Int                                -- ^ initial Tessallation-Factor
                -> (BufferObject,NumArrayIndices)  -- ^ Buffer with Data and DataDescriptor
                -> IO GLMapState
initMapShader tessFac (buf, vertDes) = do
   ! vertexSource <- B.readFile mapVertexShaderFile
   ! tessControlSource <- B.readFile mapTessControlShaderFile
   ! tessEvalSource <- B.readFile mapTessEvalShaderFile
   ! fragmentSource <- B.readFile mapFragmentShaderFile
   vertexShader <- compileShaderSource VertexShader vertexSource
   checkError "compile Vertex"
   tessControlShader <- compileShaderSource TessControlShader tessControlSource
   checkError "compile TessControl"
   tessEvalShader <- compileShaderSource TessEvaluationShader tessEvalSource
   checkError "compile TessEval"
   fragmentShader <- compileShaderSource FragmentShader fragmentSource
   checkError "compile Frag"
   program <- createProgramUsing [vertexShader, tessControlShader, tessEvalShader, fragmentShader]
   checkError "compile Program"

   currentProgram $= Just program

   projectionMatrixIndex <- get (uniformLocation program "ProjectionMatrix")
   checkError "projMat"

   viewMatrixIndex <- get (uniformLocation program "ViewMatrix")
   checkError "viewMat"

   modelMatrixIndex <- get (uniformLocation program "ModelMatrix")
   checkError "modelMat"

   normalMatrixIndex <- get (uniformLocation program "NormalMatrix")
   checkError "normalMat"

   tessLevelInner <- get (uniformLocation program "TessLevelInner")
   checkError "TessLevelInner"

   tessLevelOuter <- get (uniformLocation program "TessLevelOuter")
   checkError "TessLevelOuter"


   vertexIndex <- get (attribLocation program "Position")
   vertexAttribArray vertexIndex $= Enabled
   checkError "vertexInd"

   normalIndex <- get (attribLocation program "Normal")
   vertexAttribArray normalIndex $= Enabled
   checkError "normalInd"

   colorIndex <- get (attribLocation program "Color")
   vertexAttribArray colorIndex $= Enabled
   checkError "colorInd"

   att <- get (activeAttribs program)

   putStrLn $ unlines $ "Attributes: ":map show att
   putStrLn $ unlines $ ["Indices: ", show (colorIndex, normalIndex, vertexIndex)]

   tex <- genObjectName
   overTex <- genObjectName

   texts <- genObjectNames 6
   
   testobj <- parseIQM "sample.iqm"

   let
	objs = [GLObject testobj (Coord3D 0 10 0)]

   ! vertexSource' <- B.readFile objectVertexShaderFile
   ! fragmentSource' <- B.readFile objectFragmentShaderFile
   vertexShader' <- compileShaderSource VertexShader vertexSource'
   checkError "compile Object-Vertex"
   fragmentShader' <- compileShaderSource FragmentShader fragmentSource'
   checkError "compile Object-Fragment"
   objProgram <- createProgramUsing [vertexShader', fragmentShader']
   checkError "compile Object-Program"
   
   currentProgram $= Just objProgram

   checkError "initShader"
   return GLMapState
        { _mapProgram         = program
        , _shdrColorIndex     = colorIndex
        , _shdrNormalIndex    = normalIndex
        , _shdrVertexIndex    = vertexIndex
        , _shdrProjMatIndex   = projectionMatrixIndex
        , _shdrViewMatIndex   = viewMatrixIndex
        , _shdrModelMatIndex  = modelMatrixIndex
        , _shdrNormalMatIndex = normalMatrixIndex
        , _shdrTessInnerIndex = tessLevelInner
        , _shdrTessOuterIndex = tessLevelOuter
        , _renderedMapTexture = tex
        , _stateTessellationFactor = tessFac
        , _stateMap           = buf
        , _mapVert            = vertDes
        , _overviewTexture    = overTex
        , _mapTextures        = texts
	, _mapObjects         = objs
	, _objectProgram      = objProgram
        }

initHud :: IO GLHud
initHud = do
   ! vertexSource <- B.readFile "shaders/ui/vertex.shader"
   ! fragmentSource <- B.readFile "shaders/ui/fragment.shader"
   vertexShader <- compileShaderSource VertexShader vertexSource
   checkError "compile UI-Vertex"
   fragmentShader <- compileShaderSource FragmentShader fragmentSource
   checkError "compile UI-Fragment"
   program <- createProgramUsing [vertexShader, fragmentShader]
   checkError "compile Program"

   tex <- genObjectName

   currentProgram $= Just program

   backIndex <- get (uniformLocation program "tex[0]")
   texIndex <- get (uniformLocation program "tex[1]")
   checkError "ui-tex"

   -- simple triangle over the whole screen.
   let vertexBufferData = reverse [-1, -1, 1, -1, -1, 1, 1, 1] :: [GLfloat]

   vertexIndex <- get (attribLocation program "position")
   vertexAttribArray vertexIndex $= Enabled
   checkError "vertexInd"

   ebo <- makeBuffer ElementArrayBuffer ([0..3] :: [GLuint])
   vbo <- makeBuffer ArrayBuffer        vertexBufferData

   att <- get (activeAttribs program)

   putStrLn $ unlines $ "Attributes: ":map show att
   putStrLn $ unlines $ ["Indices: ", show (texIndex)]

   checkError "initHud"
   return GLHud
        { _hudTexture           = tex
        , _hudTexIndex          = texIndex
        , _hudBackIndex         = backIndex
        , _hudVertexIndex       = vertexIndex
        , _hudVert              = 4
        , _hudVBO               = vbo
        , _hudEBO               = ebo
        , _hudProgram           = program
        }




initRendering :: IO ()
initRendering = do
        clearColor $= Color4 0.6 0.7 0.8 1
        depthFunc $= Just Less
        glCullFace gl_BACK
        checkError "initRendering"

{-renderOverview :: Pioneers ()
renderOverview = do
    liftIO $ do
        ---- RENDER OVERVIEW MAP ------------------------------------------


        bindFramebuffer Framebuffer $= (state ^. gl.glFramebuffer)
        bindRenderbuffer Renderbuffer $= (state ^. gl.glRenderbuffer)
        framebufferRenderbuffer
                Framebuffer
                DepthAttachment
                Renderbuffer
                (state ^. gl.glRenderbuffer)
        textureBinding Texture2D $= Just (state ^. gl.glMap.renderedMapTexture)

        framebufferTexture2D
                Framebuffer
                (ColorAttachment 0)
                Texture2D
                (state ^. gl.glMap.renderedMapTexture)
                0

        -- Render to FrameBufferObject
        drawBuffers $= [FBOColorAttachment 0]
        checkError "setup Render-Target"

        clear [ColorBuffer, DepthBuffer]
        checkError "clear buffer"


        currentProgram $= Just (state ^. gl.glMap.mapProgram)

        checkError "setting up buffer"
        --set up projection (= copy from state)
        with (distribute frust) $ \ptr ->
              glUniformMatrix4fv proj 1 0 (castPtr (ptr :: Ptr (L.M44 CFloat)))
        checkError "copy projection"

        --set up camera
        let ! cam = getCam camPos zDist' xa ya
        with (distribute cam) $ \ptr ->
              glUniformMatrix4fv vmat 1 0 (castPtr (ptr :: Ptr (L.M44 CFloat)))
        checkError "copy cam"

        --set up normal--Mat transpose((model*camera)^-1)
        let normal' = (case L.inv33 (fmap (^. L._xyz) cam ^. L._xyz) of
                                             (Just a) -> a
                                             Nothing  -> L.eye3) :: L.M33 CFloat
            nmap = collect id normal' :: L.M33 CFloat --transpose...

        with (distribute nmap) $ \ptr ->
              glUniformMatrix3fv nmat 1 0 (castPtr (ptr :: Ptr (L.M33 CFloat)))

        checkError "nmat"

        glUniform1f tli (fromIntegral tessFac)
        glUniform1f tlo (fromIntegral tessFac)

        bindBuffer ArrayBuffer $= Just map'
        vertexAttribPointer ci $= fgColorIndex
        vertexAttribArray ci   $= Enabled
        vertexAttribPointer ni $= fgNormalIndex
        vertexAttribArray ni   $= Enabled
        vertexAttribPointer vi $= fgVertexIndex
        vertexAttribArray vi   $= Enabled
        checkError "beforeDraw"

        glPatchParameteri gl_PATCH_VERTICES 3

        cullFace $= Just Front

        glDrawArrays gl_PATCHES 0 (fromIntegral numVert)
        checkError "draw map"
-}


render :: Pioneers ()
render = do
    state <- RWS.get
    let xa       = state ^. camera.xAngle
        ya       = state ^. camera.yAngle
        (UniformLocation proj)  = state ^. gl.glMap.shdrProjMatIndex
        (UniformLocation nmat)  = state ^. gl.glMap.shdrNormalMatIndex
        (UniformLocation vmat)  = state ^. gl.glMap.shdrViewMatIndex
        (UniformLocation tli)   = state ^. gl.glMap.shdrTessInnerIndex
        (UniformLocation tlo)   = state ^. gl.glMap.shdrTessOuterIndex
        vi       = state ^. gl.glMap.shdrVertexIndex
        ni       = state ^. gl.glMap.shdrNormalIndex
        ci       = state ^. gl.glMap.shdrColorIndex
        numVert  = state ^. gl.glMap.mapVert
        map'     = state ^. gl.glMap.stateMap
        frust    = state ^. camera.Types.frustum
        camPos   = state ^. camera.camObject
        zDist'   = state ^. camera.zDist
        tessFac  = state ^. gl.glMap.stateTessellationFactor
    liftIO $ do
        ---- RENDER MAP IN TEXTURE ------------------------------------------

        bindFramebuffer Framebuffer $= (state ^. gl.glFramebuffer)
        bindRenderbuffer Renderbuffer $= (state ^. gl.glRenderbuffer)
        framebufferRenderbuffer
                Framebuffer
                DepthAttachment
                Renderbuffer
                (state ^. gl.glRenderbuffer)
        textureBinding Texture2D $= Just (state ^. gl.glMap.renderedMapTexture)

        framebufferTexture2D
                Framebuffer
                (ColorAttachment 0)
                Texture2D
                (state ^. gl.glMap.renderedMapTexture)
                0

        -- Render to FrameBufferObject
        drawBuffers $= [FBOColorAttachment 0]
        checkError "setup Render-Target"

        clear [ColorBuffer, DepthBuffer]
        checkError "clear buffer"


        currentProgram $= Just (state ^. gl.glMap.mapProgram)

        checkError "setting up buffer"
        --set up projection (= copy from state)
        with (distribute frust) $ \ptr ->
              glUniformMatrix4fv proj 1 0 (castPtr (ptr :: Ptr (L.M44 CFloat)))
        checkError "copy projection"

        --set up camera
        let ! cam = getCam camPos zDist' xa ya
        with (distribute cam) $ \ptr ->
              glUniformMatrix4fv vmat 1 0 (castPtr (ptr :: Ptr (L.M44 CFloat)))
        checkError "copy cam"

        --set up normal--Mat transpose((model*camera)^-1)
        let normal' = (case L.inv33 (fmap (^. L._xyz) cam ^. L._xyz) of
                                             (Just a) -> a
                                             Nothing  -> L.eye3) :: L.M33 CFloat
            nmap = collect id normal' :: L.M33 CFloat --transpose...

        with (distribute nmap) $ \ptr ->
              glUniformMatrix3fv nmat 1 0 (castPtr (ptr :: Ptr (L.M33 CFloat)))

        checkError "nmat"

        glUniform1f tli (fromIntegral tessFac)
        glUniform1f tlo (fromIntegral tessFac)

        bindBuffer ArrayBuffer $= Just map'
        vertexAttribPointer ci $= fgColorIndex
        vertexAttribArray ci   $= Enabled
        vertexAttribPointer ni $= fgNormalIndex
        vertexAttribArray ni   $= Enabled
        vertexAttribPointer vi $= fgVertexIndex
        vertexAttribArray vi   $= Enabled
        checkError "beforeDraw"

        glPatchParameteri gl_PATCH_VERTICES 3

        cullFace $= Just Front

        glDrawArrays gl_PATCHES 0 (fromIntegral numVert)


	currentProgram $= Just (state ^. gl.glMap.objectProgram)
	

        checkError "draw map"

        -- set sample 1 as target in renderbuffer
        {-framebufferRenderbuffer
                DrawFramebuffer              --write-only
                (ColorAttachment 1)          --sample 1
                Renderbuffer                 --const
                rb                              --buffer-}

        ---- COMPOSE RENDERING --------------------------------------------
        -- Render to BackBuffer (=Screen)
        bindFramebuffer Framebuffer $= defaultFramebufferObject
        drawBuffer $= BackBuffers
        -- Drawing HUD
        clear [ColorBuffer, DepthBuffer]
        checkError "clear buffer"

        let hud    = state ^. gl.glHud
            stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
            vad    = VertexArrayDescriptor 2 Float stride offset0
        currentProgram $= Just (hud ^. hudProgram)

        activeTexture  $= TextureUnit 0
        textureBinding Texture2D $= Just (hud ^. hudTexture)
        uniform (hud ^. hudTexIndex) $= Index1 (0::GLint)

        activeTexture  $= TextureUnit 1
        textureBinding Texture2D $= Just (state ^. gl.glMap.renderedMapTexture)
        uniform (hud ^. hudBackIndex) $= Index1 (1::GLint)

        bindBuffer ArrayBuffer $= Just (hud ^. hudVBO)
        vertexAttribPointer (hud ^. hudVertexIndex) $= (ToFloat, vad)
        vertexAttribArray   (hud ^. hudVertexIndex) $= Enabled

        bindBuffer ElementArrayBuffer $= Just (hud ^. hudEBO)
        drawElements TriangleStrip 4 UnsignedInt offset0


        {-let winRenderer = env ^. renderer
        tryWithTexture
                (state ^. hudTexture)                          --maybe tex
                (\tex -> renderCopy winRenderer tex Nothing Nothing) --function with "hole"
                                                       --Nothing == whole source-tex, whole dest-tex
                (return ())                                       --fail-case-}

