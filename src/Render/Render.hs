{-# LANGUAGE BangPatterns, InstanceSigs, ExistentialQuantification #-}
module Render.Render where

import qualified Data.ByteString                            as B
import           Data.Array.Storable
import qualified Data.Vector.Storable                       as V
import           Foreign.Marshal.Array                      (withArray)
import           Foreign.Storable
import           Graphics.Rendering.OpenGL.GL.BufferObjects
import           Graphics.Rendering.OpenGL.GL.Framebuffer   (clearColor)
import           Graphics.Rendering.OpenGL.GL.ObjectName
import           Graphics.Rendering.OpenGL.GL.PerFragment
import           Graphics.Rendering.OpenGL.GL.Shaders
import           Graphics.Rendering.OpenGL.GL.StateVar
import           Graphics.Rendering.OpenGL.GL.VertexArrays  (Capability (..),
                                                             vertexAttribArray,
                                                             VertexArrayDescriptor,
                                                             DataType(Float))
import           Graphics.Rendering.OpenGL.GL.VertexSpec
import           Graphics.Rendering.OpenGL.Raw.Core31
import           Render.Misc
import           Foreign.Ptr                                (Ptr, wordPtrToPtr)

import           Types
import           Graphics.GLUtil.BufferObjects              (makeBuffer)

mapVertexShaderFile :: String
mapVertexShaderFile = "shaders/map/vertex.shader"
mapTessControlShaderFile :: String
mapTessControlShaderFile = "shaders/map/tessControl.shader"
mapTessEvalShaderFile :: String
mapTessEvalShaderFile = "shaders/map/tessEval.shader"
mapFragmentShaderFile :: String
mapFragmentShaderFile = "shaders/map/fragment.shader"

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

initMapShader :: IO (
                      Program           -- ^ the GLSL-Program
                      , AttribLocation  -- ^ color
                      , AttribLocation  -- ^ normal
                      , AttribLocation  -- ^ vertex
                      , UniformLocation -- ^ ProjectionMat
                      , UniformLocation -- ^ ViewMat
                      , UniformLocation -- ^ ModelMat
                      , UniformLocation -- ^ NormalMat
                      , UniformLocation -- ^ TessLevelInner
                      , UniformLocation -- ^ TessLevelOuter
                      )
initMapShader = do
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

   checkError "initShader"
   return (program, colorIndex, normalIndex, vertexIndex, projectionMatrixIndex, viewMatrixIndex, modelMatrixIndex, normalMatrixIndex, tessLevelInner, tessLevelOuter)

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

   texIndex <- get (uniformLocation program "tex")
   checkError "ui-tex"

   -- | simple triangle over the whole screen.
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
        , _hudVertexIndex       = vertexIndex
        , _hudVert              = 4
        , _hudVBO               = vbo
        , _hudEBO               = ebo
        , _hudProgram           = program
        }
        



initRendering :: IO ()
initRendering = do
        clearColor $= Color4 0 0 0 0
        depthFunc $= Just Less
        glCullFace gl_BACK
        checkError "initRendering"
