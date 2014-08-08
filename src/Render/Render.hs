{-# LANGUAGE BangPatterns, InstanceSigs, ExistentialQuantification #-}
module Render.Render (initBuffer, initMapShader, initHud, initRendering, render) where

import qualified Data.ByteString                            as B
import           Foreign.Marshal.Array                      (withArray)
import           Foreign.Storable
import           Graphics.Rendering.OpenGL.GL
import           Graphics.Rendering.OpenGL.Raw.Core31
import           Graphics.Rendering.OpenGL.Raw.ARB.TessellationShader
import           Graphics.GLUtil.BufferObjects
import qualified Linear as L
import           Control.Lens                               ((^.))
import           Control.Monad.RWS.Strict             (liftIO)
import qualified Control.Monad.RWS.Strict as RWS      (get)
import           Control.Concurrent.STM               (readTVarIO)
import           Data.Distributive                    (distribute, collect)
-- FFI
import           Foreign                              (Ptr, castPtr, with)
import           Foreign.C                            (CFloat)

import           Map.Graphics
import           Types
import           Render.Misc
import           Render.Types
import           Importer.IQM.Parser
import           Importer.IQM.Types

mapVertexShaderFile :: String
mapVertexShaderFile = "shaders/map/vertex.shader"
mapTessControlShaderFile :: String
mapTessControlShaderFile = "shaders/map/tessControl.shader"
mapTessEvalShaderFile :: String
mapTessEvalShaderFile = "shaders/map/tessEval.shader"
mapFragmentShaderFile :: String
mapFragmentShaderFile = "shaders/map/fragment.shader"
mapFragmentShaderShadowMapFile :: String
mapFragmentShaderShadowMapFile = "shaders/map/fragmentShadow.shader"

objectVertexShaderFile :: String
objectVertexShaderFile = "shaders/mapobjects/vertex.shader"
objectFragmentShaderFile :: String
objectFragmentShaderFile = "shaders/mapobjects/fragment.shader"

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
                -> IO (GLMapState, TextureObject)
initMapShader tessFac (buf, vertDes) = do
   ! vertexSource <- B.readFile mapVertexShaderFile
   ! tessControlSource <- B.readFile mapTessControlShaderFile
   ! tessEvalSource <- B.readFile mapTessEvalShaderFile
   ! fragmentSource <- B.readFile mapFragmentShaderFile
   ! fragmentShadowSource <- B.readFile mapFragmentShaderShadowMapFile
   vertexShader <- compileShaderSource VertexShader vertexSource
   checkError "compile Vertex"
   tessControlShader <- compileShaderSource TessControlShader tessControlSource
   checkError "compile TessControl"
   tessEvalShader <- compileShaderSource TessEvaluationShader tessEvalSource
   checkError "compile TessEval"
   fragmentShader <- compileShaderSource FragmentShader fragmentSource
   checkError "compile Frag"
   fragmentShadowShader <- compileShaderSource FragmentShader fragmentShadowSource
   checkError "compile Frag"
   program <- createProgramUsing [vertexShader, tessControlShader, tessEvalShader, fragmentShader]
   shadowProgram <- createProgramUsing [vertexShader, tessControlShader, tessEvalShader, fragmentShadowShader]
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

   putStrLn $ unlines $ "Map-Attributes: ":map show att
   putStrLn $ unlines ["Map-Indices: ", show (colorIndex, normalIndex, vertexIndex)]

   tex <- genObjectName
   overTex <- genObjectName

   textures <- genObjectNames 6

   smap <- genObjectName

   testobj <- parseIQM "models/box.iqm"

   let
    objs = [MapObject testobj (L.V3 0 10 0) (MapObjectState ())]

   currentProgram $= Nothing

   ! vertexSource' <- B.readFile objectVertexShaderFile
   ! fragmentSource' <- B.readFile objectFragmentShaderFile
   vertexShader' <- compileShaderSource VertexShader vertexSource'
   checkError "compile Object-Vertex"
   fragmentShader' <- compileShaderSource FragmentShader fragmentSource'
   checkError "compile Object-Fragment"
   objProgram <- createProgramUsing [vertexShader', fragmentShader']
   checkError "compile Object-Program"

   currentProgram $= Just objProgram

   vertexIndex' <- get (attribLocation objProgram "Position")
   vertexAttribArray vertexIndex $= Enabled
   checkError "Object-vertexInd"

   normalIndex' <- get (attribLocation objProgram "Normal")
   vertexAttribArray normalIndex $= Enabled
   checkError "Object-normalInd"

   texIndex' <- get (attribLocation objProgram "TexCoord")
   vertexAttribArray colorIndex $= Enabled
   checkError "Object-texInd"

   projectionMatrixIndex' <- get (uniformLocation objProgram "ProjectionMatrix")
   checkError "projMat"

   viewMatrixIndex' <- get (uniformLocation objProgram "ViewMatrix")
   checkError "viewMat"

   modelMatrixIndex' <- get (uniformLocation objProgram "ModelMatrix")
   checkError "modelMat"

   normalMatrixIndex' <- get (uniformLocation objProgram "NormalMatrix")
   checkError "normalMat"

   --tessLevelInner' <- get (uniformLocation objProgram "TessLevelInner")
   --checkError "TessLevelInner"

   --tessLevelOuter' <- get (uniformLocation objProgram "TessLevelOuter")
   --checkError "TessLevelOuter"

   vertexOffsetIndex' <- get (uniformLocation objProgram "PositionOffset")
   checkError "PositionOffset"

   att' <- get (activeAttribs objProgram)
   putStrLn $ unlines $ "Model-Attributes: ":map show att'
   uni' <- get (activeUniforms objProgram)
   putStrLn $ unlines $ "Model-Uniforms: ":map show uni'
   putStrLn $ unlines $ ["Model-Indices: ", show (texIndex', normalIndex', vertexIndex')]
   checkError "initShader"
   let sdata = MapShaderData
            { shdrVertexIndex      = vertexIndex
            , shdrColorIndex       = colorIndex
            , shdrNormalIndex      = normalIndex
            , shdrProjMatIndex     = projectionMatrixIndex
            , shdrViewMatIndex     = viewMatrixIndex
            , shdrModelMatIndex    = modelMatrixIndex
            , shdrNormalMatIndex   = normalMatrixIndex
            , shdrTessInnerIndex   = tessLevelInner
            , shdrTessOuterIndex   = tessLevelOuter
            }

   let smodata = MapObjectShaderData
            { shdrMOVertexIndex    = vertexIndex'
            , shdrMOVertexOffsetIndex = vertexOffsetIndex'
            , shdrMONormalIndex    = normalIndex'
            , shdrMOTexIndex       = texIndex'
            , shdrMOProjMatIndex   = projectionMatrixIndex'
            , shdrMOViewMatIndex   = viewMatrixIndex'
            , shdrMOModelMatIndex  = modelMatrixIndex'
            , shdrMONormalMatIndex = normalMatrixIndex'
            , shdrMOTessInnerIndex = UniformLocation 0 --tessLevelInner'
            , shdrMOTessOuterIndex = UniformLocation 0 --tessLevelOuter'
            }

   return (GLMapState
        { _mapProgram         = program
        , _mapShaderData      = sdata
        , _mapObjectShaderData = smodata
        , _stateTessellationFactor = tessFac
        , _stateMap           = buf
        , _mapVert            = vertDes
        , _overviewTexture    = overTex
        , _mapTextures        = textures
        , _shadowMapTexture   = smap
        , _mapObjects         = objs
        , _objectProgram      = objProgram
        , _shadowMapProgram   = shadowProgram
        }, tex)

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
   putStrLn $ unlines $ ["Indices: ", show texIndex]

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


-- | renders an IQM-Model at Position with scaling
renderIQM :: IQM -> L.V3 CFloat -> L.V3 CFloat -> IO ()
renderIQM m p@(L.V3 x y z) s@(L.V3 sx sy sz) = do
    bindVertexArrayObject $= Just (vertexArrayObject m)
    let n = fromIntegral.num_triangles.header $ m
    --print $ concat ["drawing ", show n," triangles"]
    drawElements Triangles n UnsignedInt (triangles m)
    checkError "drawing model"
    return ()

renderObject :: MapObject -> IO ()
renderObject (MapObject model pos@(L.V3 x y z) _{-state-}) =
    renderIQM model pos (L.V3 1 1 1)

drawMap :: Pioneers ()
drawMap = do
    state <- RWS.get
    let
        d        = state ^. gl.glMap.mapShaderData
        vi       = shdrVertexIndex d
        ni       = shdrNormalIndex d
        ci       = shdrColorIndex d
        numVert  = state ^. gl.glMap.mapVert
        map'     = state ^. gl.glMap.stateMap
        tessFac  = state ^. gl.glMap.stateTessellationFactor
        (UniformLocation tli)   = shdrTessInnerIndex d
        (UniformLocation tlo)   = shdrTessOuterIndex d
    liftIO $ do
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

        cullFace $= Nothing --Just Front
        polygonMode $= (Fill,Fill)

        glDrawArrays gl_PATCHES 0 (fromIntegral numVert)

        checkError "draw map"

        -- set sample 1 as target in renderbuffer
        {-framebufferRenderbuffer
                DrawFramebuffer              --write-only
                (ColorAttachment 1)          --sample 1
                Renderbuffer                 --const
                rb                              --buffer-}
mat44ToGPU :: L.M44 CFloat -> UniformLocation -> String -> IO ()
mat44ToGPU mat (UniformLocation dest) name = do
        with (distribute mat) $ \ptr ->
              glUniformMatrix4fv dest 1 0 (castPtr (ptr :: Ptr (L.M44 CFloat)))
        checkError $ "copy Matrix (" ++ name ++ ")"

mat33ToGPU :: L.M33 CFloat -> UniformLocation -> String -> IO ()
mat33ToGPU mat (UniformLocation dest) name = do
        with (distribute mat) $ \ptr ->
              glUniformMatrix3fv dest 1 0 (castPtr (ptr :: Ptr (L.M33 CFloat)))
        checkError $ "copy Matrix (" ++ name ++ ")"


render :: Pioneers ()
render = do
    -- -- FOO <<<<<<<<< denotes a stage (Shadowmap, Map, UI)
    -- -- BAR --------- denotes a substage (which parts etc.)
    -- -- BAZ           denotes a sub-substage
    state <- RWS.get
    cam <- liftIO $ readTVarIO (state ^. camera)
    let xa       = cam ^. xAngle
        ya       = cam ^. yAngle
        frust    = cam ^. Types.frustum
        camPos   = cam ^. camObject
        zDist'   = cam ^. zDist
        d        = state ^. gl.glMap.mapShaderData
        proj  = shdrProjMatIndex d
        nmat  = shdrNormalMatIndex d
        vmat  = shdrViewMatIndex d
        dmo      = state ^. gl.glMap.mapObjectShaderData
        projmo = shdrMOProjMatIndex dmo
        nmatmo = shdrMONormalMatIndex dmo
        vmatmo = shdrMOViewMatIndex dmo
        suncam = getCam camPos 1 0.7 0 --TODO: Fix position of sun
        sunnormal' = (case L.inv33 (fmap (^. L._xyz) suncam ^. L._xyz) of
                                         (Just a) -> a
                                         Nothing  -> L.eye3) :: L.M33 CFloat
        sunnmap = collect id sunnormal' :: L.M33 CFloat --transpose...
        cam' = getCam camPos zDist' xa ya
        normal' = (case L.inv33 (fmap (^. L._xyz) cam' ^. L._xyz) of
                                         (Just a) -> a
                                         Nothing  -> L.eye3) :: L.M33 CFloat
        nmap = collect id normal' :: L.M33 CFloat --transpose...

    liftIO $ do

        bindFramebuffer Framebuffer $= (state ^. gl.glFramebuffer)
        {-bindRenderbuffer Renderbuffer $= (state ^. gl.glRenderbuffer)
        framebufferRenderbuffer
                Framebuffer
                DepthAttachment
                Renderbuffer
                (state ^. gl.glRenderbuffer)-}

        ---- RENDER SHADOWMAP <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        textureBinding Texture2D $= Just (state ^. gl.glMap.shadowMapTexture)
        framebufferTexture2D
                Framebuffer
                DepthAttachment
                Texture2D
                (state ^. gl.glMap.shadowMapTexture)
                0

        drawBuffer $= NoBuffers --color-buffer is not needed but must(?) be set up
        checkError "setup Render-Target"

        clear [DepthBuffer]
        checkError "clearing shadowmap-buffer"

        currentProgram $= Just (state ^. gl.glMap.mapProgram)
        checkError "setting up shadowmap-program"

        --set up projection (= copy from state)
        --TODO: Fix width/depth
        mat44ToGPU (createFrustumOrtho 20 20 0 100) proj "shadowmap-projection"
        --set up camera
        mat44ToGPU suncam vmat "shadowmap-cam"
        --set up normal--Mat transpose((model*camera)^-1)
        --TODO: needed?
        mat33ToGPU sunnmap nmat "nmat"

    drawMap

    liftIO $ do
        ---- RENDER MAPOBJECTS --------------------------------------------
        currentProgram $= Just (state ^. gl.glMap.objectProgram)
        checkError "setting up shadowmap-program"

        --set up projection (= copy from state)
        --TODO: Fix width/depth
        mat44ToGPU (createFrustumOrtho 20 20 0 100) projmo "shadowmap-projection"

        --set up camera
        --TODO: Fix magic constants... and camPos
        mat44ToGPU suncam vmatmo "shadowmap-camera"

        --set up normal--Mat transpose((model*camera)^-1)
        --needed?
        mat33ToGPU sunnmap nmatmo "nmat"

        mapM_ renderObject (state ^. gl.glMap.mapObjects)
        checkError "draw mapobjects"

        checkError "draw ShadowMap"

        ---- RENDER MAP IN TEXTURE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        -- COLORMAP
        tex <- liftIO $ readTVarIO (state ^. mapTexture)
        textureBinding Texture2D $= Just tex
        framebufferTexture2D
                Framebuffer
                (ColorAttachment 0)
                Texture2D
                tex
                0

        -- Render to FrameBufferObject
        drawBuffers $= [FBOColorAttachment 0]
        checkError "setup Render-Target"

        clear [ColorBuffer, DepthBuffer]
        checkError "clear buffer"

        currentProgram $= Just (state ^. gl.glMap.mapProgram)
        checkError "setting up buffer"
        --set up projection (= copy from state)
        mat44ToGPU frust proj "projection"
        --set up camera
        mat44ToGPU cam' vmat "camera"
        --set up normal--Mat transpose((model*camera)^-1)
        mat33ToGPU nmap nmat "nmat"

    drawMap
    liftIO $ do
        ---- RENDER MAPOBJECTS --------------------------------------------
        checkError "clear buffer"
        currentProgram $= Just (state ^. gl.glMap.objectProgram)
        checkError "setting up shadowmap-program"
        --set up projection (= copy from state)
        mat44ToGPU frust projmo "mapObjects-projection"
        --set up camera
        mat44ToGPU cam' vmatmo "mapObjects-cam"
        --set up normal
        mat33ToGPU nmap nmatmo "mapObjects-nmat"

        mapM_ renderObject (state ^. gl.glMap.mapObjects)
        checkError "draw mapobjects"

        ---- COMPOSE RENDERING --------------------------------------------
        -- Render to BackBuffer (=Screen)
        bindFramebuffer Framebuffer $= defaultFramebufferObject
        drawBuffer $= BackBuffers
        -- Drawing HUD
        clear [ColorBuffer, DepthBuffer]
        checkError "clear buffer"
        polygonMode $= (Fill,Fill)

        let hud    = state ^. gl.glHud
            stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
            vad    = VertexArrayDescriptor 2 Float stride offset0
        currentProgram $= Just (hud ^. hudProgram)

        activeTexture  $= TextureUnit 0
        textureBinding Texture2D $= Just (hud ^. hudTexture)
        uniform (hud ^. hudTexIndex) $= Index1 (0::GLint)

        activeTexture  $= TextureUnit 1
        tex <- liftIO $ readTVarIO (state ^. mapTexture)
        textureBinding Texture2D $= Just tex
        uniform (hud ^. hudBackIndex) $= Index1 (1::GLint)

        bindBuffer ArrayBuffer $= Just (hud ^. hudVBO)
        vertexAttribPointer (hud ^. hudVertexIndex) $= (ToFloat, vad)
        vertexAttribArray   (hud ^. hudVertexIndex) $= Enabled

        bindBuffer ElementArrayBuffer $= Just (hud ^. hudEBO)
        drawElements TriangleStrip 4 UnsignedInt offset0

