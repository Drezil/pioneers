{-# LANGUAGE BangPatterns #-}
module Render.Render where

import qualified Data.ByteString                            as B
import           Foreign.Marshal.Array                      (withArray)
import           Foreign.Storable                           (sizeOf)
import           Graphics.Rendering.OpenGL.GL.BufferObjects
import           Graphics.Rendering.OpenGL.GL.Framebuffer   (clearColor)
import           Graphics.Rendering.OpenGL.GL.ObjectName
import           Graphics.Rendering.OpenGL.GL.PerFragment
import           Graphics.Rendering.OpenGL.GL.Shaders
import           Graphics.Rendering.OpenGL.GL.StateVar
import           Graphics.Rendering.OpenGL.GL.VertexArrays  (Capability (..),
                                                             vertexAttribArray)
import           Graphics.Rendering.OpenGL.GL.VertexSpec
import           Graphics.Rendering.OpenGL.Raw.Core31
import           Render.Misc

vertexShaderFile :: String
vertexShaderFile = "shaders/vertex.shader"
tessControlShaderFile :: String
tessControlShaderFile = "shaders/tessControl.shader"
tessEvalShaderFile :: String
tessEvalShaderFile = "shaders/tessEval.shader"
fragmentShaderFile :: String
fragmentShaderFile = "shaders/fragment.shader"

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

initShader :: IO (
                        AttribLocation  -- ^ color
                      , AttribLocation  -- ^ normal
                      , AttribLocation  -- ^ vertex
                      , UniformLocation -- ^ ProjectionMat
                      , UniformLocation -- ^ ViewMat
                      , UniformLocation -- ^ ModelMat
                      , UniformLocation -- ^ NormalMat
                      )
initShader = do
   ! vertexSource <- B.readFile vertexShaderFile
   ! tessControlSource <- B.readFile tessControlShaderFile
   ! tessEvalSource <- B.readFile tessEvalShaderFile
   ! fragmentSource <- B.readFile fragmentShaderFile
   vertexShader <- compileShaderSource VertexShader vertexSource
   checkError "compile Vertex"
   tessControlShader <- compileShaderSource TessControlShader tessControlSource
   checkError "compile Vertex"
   tessEvalShader <- compileShaderSource TessEvaluationShader tessEvalSource
   checkError "compile Vertex"
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
   return (colorIndex, normalIndex, vertexIndex, projectionMatrixIndex, viewMatrixIndex, modelMatrixIndex, normalMatrixIndex)

initRendering :: IO ()
initRendering = do
        clearColor $= Color4 0 0 0 0
        depthFunc $= Just Less
        glCullFace gl_BACK
        checkError "initRendering"
