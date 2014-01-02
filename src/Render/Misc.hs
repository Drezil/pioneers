module Render.Misc where

import           Control.Monad
import qualified Data.ByteString                            as B (ByteString)
import           Graphics.Rendering.OpenGL.GL.Shaders
import           Graphics.Rendering.OpenGL.GL.StateVar
import           Graphics.Rendering.OpenGL.GL.StringQueries
import           Graphics.Rendering.OpenGL.GLU.Errors
import           System.IO                                  (hPutStrLn, stderr)


checkError :: String -> IO ()
checkError functionName = get errors >>= mapM_ reportError
   where reportError e =
            hPutStrLn stderr (showError e ++ " detected in " ++ functionName)
         showError (Error category message) =
            "GL error " ++ show category ++ " (" ++ message ++ ")"

dumpInfo :: IO ()
dumpInfo = do
        let dump message var = putStrLn . ((message ++ ": ") ++) =<< get var
        dump "Vendor" vendor
        dump "Renderer" renderer
        dump "Version" glVersion
        dump "GLSL" shadingLanguageVersion
        checkError "dumpInfo"

checked :: (t -> IO ()) -> (t -> GettableStateVar Bool) -> (t -> GettableStateVar String) -> String -> t -> IO ()
checked action getStatus getInfoLog message object = do
   action object
   status <- get (getStatus object)
   unless status $
      hPutStrLn stderr . ((message ++ " log: ") ++) =<< get (getInfoLog object)

compileAndCheck :: Shader -> IO ()
compileAndCheck = checked compileShader compileStatus shaderInfoLog "compile"

compileShaderSource :: ShaderType -> B.ByteString -> IO Shader
compileShaderSource st source = do
   shader <- createShader st
   shaderSourceBS shader $= source
   compileAndCheck shader
   return shader

linkAndCheck :: Program -> IO ()
linkAndCheck = checked linkProgram linkStatus programInfoLog "link"

createProgramUsing :: [Shader] -> IO Program
createProgramUsing shaders = do
   program <- createProgram
   attachedShaders program $= shaders
   linkAndCheck program
   return program
