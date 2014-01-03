module Render.Misc where

import           Control.Monad
import qualified Data.ByteString                            as B (ByteString)
import           Graphics.Rendering.OpenGL.GL.Shaders
import           Graphics.Rendering.OpenGL.GL.StateVar
import           Graphics.Rendering.OpenGL.GL.StringQueries
import           Graphics.Rendering.OpenGL.GLU.Errors
import           System.IO                                  (hPutStrLn, stderr)
import Graphics.Rendering.OpenGL.Raw.Core31
import Foreign.Marshal.Array (allocaArray, pokeArray)


up :: (Double, Double, Double)
up = (0.0, 1.0, 1.0)

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

lookAtUniformMatrix4fv :: (Double, Double, Double)  --origin
                        -> (Double, Double, Double) --camera-pos
                        -> (Double, Double, Double) --up
                        -> GLint -> GLsizei -> IO () --rest of GL-call
lookAtUniformMatrix4fv o c u num size = allocaArray 16 $ \projMat ->
                                                do
                                                        pokeArray projMat $ lookAt o c u
                                                        glUniformMatrix4fv num size 1 projMat

-- generats 4x4-Projection-Matrix
lookAt :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double) -> [GLfloat]
lookAt origin eye up = 
        map (fromRational . toRational) [
         xx, yx, zx, 0,
         xy, yy, zy, 0,
         xz, yz, zz, 0,
         -(x *. eye), -(y *. eye), -(z *. eye), 1
        ]
        where
                z@(zx,zy,zz) = normal (origin .- eye)
                x@(xx,xy,xz) = normal (up *.* z)
                y@(yx,yy,yz) = z *.* x

normal :: (Double, Double, Double) -> (Double, Double, Double)
normal x = (1.0 / (sqrt (x *. x))) .* x

infixl 5 .*
--scaling
(.*) :: Double -> (Double, Double, Double) -> (Double, Double, Double)
a .* (x,y,z) = (a*x, a*y, a*z)

infixl 5 .-
--subtraction
(.-) :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
(a,b,c) .- (x,y,z) = (a-x, b-y, c-z)

infixl 5 *.*
--cross-product for left-hand-system
(*.*) :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
(a,b,c) *.* (x,y,z) = (   c*y - b*z
                        , a*z - c*x
                        , b*x - a*y
                        )

infixl 5 *.
--dot-product
(*.) :: (Double, Double, Double) -> (Double, Double, Double) -> Double
(a,b,c) *. (x,y,z) = a*x + b*y + c*z

