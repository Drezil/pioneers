{-# LANGUAGE BangPatterns #-}
module Render.Render where

import qualified Data.ByteString                            as B
import           Foreign.Marshal.Array                      (withArray)
import           Foreign.Storable                           (sizeOf)
import           Graphics.Rendering.OpenGL.GL.BufferObjects
import           Graphics.Rendering.OpenGL.GL.Framebuffer   (clearColor)
import           Graphics.Rendering.OpenGL.GL.ObjectName
import           Graphics.Rendering.OpenGL.GL.Shaders
import           Graphics.Rendering.OpenGL.GL.StateVar
import           Graphics.Rendering.OpenGL.GL.VertexArrays  (Capability (..),
                                                             vertexAttribArray)
import           Graphics.Rendering.OpenGL.GL.VertexSpec
import           Graphics.Rendering.OpenGL.Raw.Core31.Types (GLfloat)
import           Render.Misc

vertexShaderFile :: String
vertexShaderFile = "shaders/vertex.shader"
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

initShader :: IO (AttribLocation, AttribLocation, AttribLocation, UniformLocation, UniformLocation)
initShader = do
   ! vertexSource <- B.readFile vertexShaderFile
   ! fragmentSource <- B.readFile fragmentShaderFile
   vertexShader <- compileShaderSource VertexShader vertexSource
   checkError "compile Vertex"
   fragmentShader <- compileShaderSource FragmentShader fragmentSource
   checkError "compile Frag"
   program <- createProgramUsing [vertexShader, fragmentShader]
   checkError "compile Program"

   currentProgram $= Just program

   projectionMatrixIndex <- get (uniformLocation program "fg_ProjectionMatrix")
   checkError "projMat"

   modelMatrixIndex <- get (uniformLocation program "fg_ModelMatrix")
   checkError "modelMat"

   vertexIndex <- get (attribLocation program "fg_VertexIn")
   vertexAttribArray vertexIndex $= Enabled
   checkError "vertexInd"

   normalIndex <- get (attribLocation program "fg_NormalIn")
   vertexAttribArray normalIndex $= Enabled
   checkError "normalInd"

   colorIndex <- get (attribLocation program "fg_Color")
   vertexAttribArray colorIndex $= Enabled
   checkError "colorInd"

   att <- get (activeAttribs program)

   putStrLn $ unlines $ "Attributes: ":map show att
   putStrLn $ unlines $ ["Indices: ", show (colorIndex, normalIndex, vertexIndex)]

   checkError "initShader"
   return (colorIndex, normalIndex, vertexIndex, projectionMatrixIndex, modelMatrixIndex)

initRendering :: IO ()
initRendering = do
        clearColor $= Color4 0 0 0 0
        checkError "initRendering"
