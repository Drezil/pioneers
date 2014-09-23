module Render.Misc where

import           Control.Monad
import qualified Data.ByteString                            as B (ByteString)
import           Data.Int                                   (Int8)
import           Data.Word                                  (Word32,Word8)
import           Graphics.Rendering.OpenGL.GL.Shaders
import           Graphics.Rendering.OpenGL.GL.StateVar
import           Graphics.Rendering.OpenGL.GL.StringQueries
import           Graphics.Rendering.OpenGL.GLU.Errors
import           Graphics.Rendering.OpenGL.GL.VertexArrayObjects
import           Graphics.Rendering.OpenGL.GL.VertexArrays
import           Graphics.Rendering.OpenGL.GL.VertexSpec
import           Graphics.Rendering.OpenGL.GL.BufferObjects
import           Graphics.UI.SDL.Types                      (Texture)
import           System.IO                                  (hPutStrLn, stderr)
import Linear
import Foreign.C (CFloat, CUChar)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, castPtr)

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

createFrustum :: Float -> Float -> Float -> Float -> M44 CFloat
createFrustum fov n' f' rat =
                let
                    ff = fromRational.toRational
                    f = ff f'
                    n = ff n'
                    s = ff $ recip (tan $ fov*0.5 * pi / 180)
                    (ratw,rath) = if rat > 1 then
                                        (1,1/ff rat)
                                  else
                                        (ff rat,1)
                in
                    V4 (V4 (s/ratw)     0            0                   0)
                       (V4    0     (s/rath)         0                   0)
                       (V4    0         0    (-((f+n)/(f-n)))  (-((2*f*n)/(f-n))))
                       (V4    0         0          (-1)                  0)

-- | Creates an orthogonal frustum with given width, height, near and far-plane
createFrustumOrtho :: Float -> Float -> Float -> Float -> M44 CFloat
createFrustumOrtho w' h' n' f' =
                 let [w,h,n,f] = map realToFrac [w',h',n',f']
                 in
                    V4 (V4 (0.5/w)    0        0        0)
                       (V4    0    (0.5/h)     0        0)
                       (V4    0       0    (-2/(f-n))   ((-f+n)/(f-n)))
                       (V4    0       0        0        1)

-- from vmath.h
lookAt :: V3 CFloat -> V3 CFloat -> V3 CFloat -> M44 CFloat
lookAt eye center up' =
        V4
         (V4 xx xy xz (-dot x eye))
         (V4 yx yy yz (-dot y eye))
         (V4 zx zy zz (-dot z eye))
         (V4 0 0 0 1)
        where
                z@(V3 zx zy zz) = normalize (eye ^-^ center)
                x@(V3 xx xy xz) = normalize (cross up' z)
                y@(V3 yx yy yz) = normalize (cross z x)


{-getCam :: (Double, Double) -- ^ Target in x/z-Plane
          -> Double        -- ^ Distance from Target
          -> Double        -- ^ Angle around X-Axis (angle down/up)
          -> Double        -- ^ Angle around Y-Axis (angle left/right)
          -> M44 CFloat

getCam (x',z') dist' xa' ya' = lookAt (cpos ^+^ at') at' up
                     where
                        at'   = V3 x 0 z
                        cpos  = crot !* (V3 0 0 (-dist))
                        crot  = (
                                (fromQuaternion $ axisAngle upmap (xa::CFloat))
                                !*!
                                (fromQuaternion $ axisAngle (V3 0 1 0) (ya::CFloat))
                                ) ::M33 CFloat
                        upmap = ((fromQuaternion $ axisAngle (V3 0 1 0) (ya::CFloat)) :: M33 CFloat)
                                !* (V3 1 0 0)
                        x     = realToFrac x'
                        z     = realToFrac z'
                        dist  = realToFrac dist'
                        xa    = realToFrac xa'
                        ya    = realToFrac ya'-}

-- | Prints any Pointer as Float-Array with given number of elements and chunks.
printPtrAsFloatArray :: Ptr a -> Int -> Int -> IO ()
printPtrAsFloatArray pointer num co = do
                        a <- peekArray num (castPtr pointer :: Ptr CFloat)
                        print $ chunksOf co a

-- | Prints any Pointer as UByte-Array with given number of elements and chunks.
printPtrAsUByteArray :: Ptr a -> Int -> Int -> IO ()
printPtrAsUByteArray pointer num co = do
                        a <- peekArray num (castPtr pointer :: Ptr CUChar)
                        print $ chunksOf co a

-- | Prints any Pointer as Word32-Array with given number of elements and chunks.
printPtrAsWord32Array :: Ptr a -> Int -> Int -> IO ()
printPtrAsWord32Array pointer num co = do
                        a <- peekArray num (castPtr pointer :: Ptr Word32)
                        print $ chunksOf co a

curb :: Ord a => a -> a -> a -> a
curb l h x
  | x < l     = l
  | x > h     = h
  | otherwise = x


tryWithTexture :: Maybe Texture -> (Texture -> a) -> a -> a
tryWithTexture t f fail' =
        case t of
                Just tex -> f tex
                _ -> fail'

genColorData ::      Int  -- ^ Amount
                -> [Word8] -- ^ [r,g,b,a], [r,g,b] - whatever should be repeatet.
                -> [Word8]
genColorData n c = take (length c*n) (cycle c)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf a xs = take a xs : chunksOf a (drop a xs)


withVAO :: VertexArrayObject -> IO a -> IO a
withVAO v a = do
                bindVertexArrayObject $= Just v
                ret <- a
                bindVertexArrayObject $= Nothing
                return ret

withVBO :: BufferObject -> BufferTarget -> IO a -> IO a
withVBO b t a = do
                bindBuffer t $= Just b
                ret <- a
                bindBuffer t $= Nothing
                return ret

withVAA :: [AttribLocation] -> IO a -> IO a
withVAA atts action = do
                mapM_ (\a -> vertexAttribArray a $= Enabled) atts
                ret <- action
                mapM_ (\a -> vertexAttribArray a $= Disabled) atts
                return ret
