module Render.Misc where

import           Control.Monad
import qualified Data.ByteString                            as B (ByteString)
import           Foreign.Marshal.Array                      (allocaArray,
                                                             pokeArray)
import Foreign.C (CFloat)
import           Graphics.Rendering.OpenGL.GL.Shaders
import           Graphics.Rendering.OpenGL.GL.StateVar
import           Graphics.Rendering.OpenGL.GL.StringQueries
import           Graphics.Rendering.OpenGL.GLU.Errors
import           Graphics.Rendering.OpenGL.Raw.Core31
import           System.IO                                  (hPutStrLn, stderr)
import Linear

up :: V3 CFloat
up = V3 0 1 0

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

createFrustum :: Float -> Float -> Float -> Float -> [GLfloat]
createFrustum fov n f rat =
                let s = recip (tan $ fov*0.5 * pi / 180) in

                map (fromRational . toRational) [
                        rat*s,0,0,0,
                        0,rat*s,0,0,
                        0,0,-(f/(f-n)), -1,
                        0,0,-((f*n)/(f-n)), 1
                ]

lookAtUniformMatrix4fv :: (Double, Double, Double)  --origin
                        -> (Double, Double, Double) --camera-pos
                        -> (Double, Double, Double) --up
                        -> [GLfloat]                --frustum
                        -> GLint -> GLsizei -> IO () --rest of GL-call
lookAtUniformMatrix4fv o c u frust num size = allocaArray 16 $ \projMat ->
                                                do
                                                        pokeArray projMat $
                                                                [1,  0,  0,  0,
                                                                 0,  0,  1,  0,
                                                                 0,  1,  0,  0,
                                                                 0,  0,  0,  1
                                                                ]
                                                                --(lookAt o c u) >< frust
                                                        glUniformMatrix4fv num size 1 projMat

infixl 5 ><

(><) :: [GLfloat] -> [GLfloat] -> [GLfloat]

[   aa, ab, ac, ad,
    ba, bb, bc, bd,
    ca, cb, cc, cd,
    da, db, dc, dd
        ] ><
  [
    xx, xy, xz, xw,
    yx, yy, yz, yw,
    zx, zy, zz, zw,
    wx, wy, wz, ww
        ] = [
                --first row
                aa*xx + ab*yx + ac*zx + ad * wx,
                aa*xy + ab*yy + ac*zy + ad * wy,
                aa*xz + ab*yz + ac*zz + ad * wz,
                aa*xw + ab*yw + ac*zw + ad * ww,

                --second row
                ba*xx + bb*yx + bc*zx + bd * wx,
                ba*xy + bb*yy + bc*zy + bd * wy,
                ba*xz + bb*yz + bc*zz + bd * wz,
                ba*xw + bb*yw + bc*zw + bd * ww,

                --third row
                ca*xx + cb*yx + cc*zx + cd * wx,
                ca*xy + cb*yy + cc*zy + cd * wy,
                ca*xz + cb*yz + cc*zz + cd * wz,
                ca*xw + cb*yw + cc*zw + cd * ww,

                --fourth row
                da*xx + db*yx + dc*zx + dd * wx,
                da*xy + db*yy + dc*zy + dd * wy,
                da*xz + db*yz + dc*zz + dd * wz,
                da*xw + db*yw + dc*zw + dd * ww
                ]
_ >< _ = error "non-conformat matrix-multiplication"


-- from vmath.h
lookAt :: V3 CFloat -> V3 CFloat -> V3 CFloat -> M44 CFloat
lookAt eye@(V3 ex ey ez) center up =
        V4
         (V4 xx yx (-zx) 0)
         (V4 xy yy (-zy) 0)
         (V4 xz yz (-zz) 0)
         (V4 0 0 0 1)
         !*!
        V4
         (V4 1 0 0 (-ex))
         (V4 0 1 0 (-ey))
         (V4 0 0 1 (-ez))
         (V4 0 0 0 1)
        where
                z@(V3 zx zy zz) = normalize (center ^-^ eye)
                x@(V3 xx xy xz) = cross z (normalize up)
                y@(V3 yx yy yz) = cross x z

-- generates 4x4-Projection-Matrix
lookAt_ :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double) -> [GLfloat]
lookAt_ at eye up =
        map (fromRational . toRational) [
         xx, yx, zx, 0,
         xy, yy, zy, 0,
         xz, yz, zz, 0,
         -(x *. eye), -(y *. eye), -(z *. eye), 1
        ]
        where
                z@(zx,zy,zz) = normal (at .- eye)
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

