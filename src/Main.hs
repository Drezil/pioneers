{-# LANGUAGE BangPatterns #-}
module Main where

-- Monad-foo
import           Control.Applicative
import           Control.Monad                        (unless, void, when)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
-- data consistency
import           Control.Concurrent.STM               (TQueue, atomically,
                                                       newTQueueIO,
                                                       tryReadTQueue,
                                                       writeTQueue)
import           Control.Monad.RWS.Strict             (RWST, ask, asks,
                                                       evalRWST, get, liftIO,
                                                       modify, put)
-- FFI
import           Foreign                              (Ptr, castPtr, with)
import           Foreign.C                            (CFloat)

-- Math
import           Control.Lens                         (transposeOf, (^.))
import           Linear                               as L

-- GUI
import           Graphics.UI.SDL

-- Render
import qualified Graphics.Rendering.OpenGL.GL         as GL
import           Graphics.Rendering.OpenGL.Raw.Core31

-- Our modules
import           Map.Map
import           Render.Misc                          (checkError,
                                                       createFrustum, getCam,
                                                       lookAt, up)
import           Render.Render                        (initRendering,
                                                       initShader)


main :: IO ()
main = return ()

