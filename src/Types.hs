module Types where

import           Control.Concurrent.STM               (TQueue)
import qualified Graphics.Rendering.OpenGL.GL         as GL
import           Graphics.UI.SDL                      as SDL
import           Foreign.C                            (CFloat)
import           Data.Time                            (UTCTime)
import Linear.Matrix (M44)
import Control.Monad.RWS.Strict (RWST)
import Graphics.UI.SDL.TTF.Types as TTF




data ArrowKeyState = ArrowKeyState {
         arrowUp      :: !Bool
        ,arrowDown    :: !Bool
        ,arrowLeft    :: !Bool
        ,arrowRight   :: !Bool
}

--Static Read-Only-State
data Env = Env
    { envEventsChan    :: TQueue Event
    , envWindow        :: !Window
    , envZDistClosest  :: !Double
    , envZDistFarthest :: !Double
    --, envGLContext     :: !GLContext
    , envFont          :: TTF.TTFFont
    }

--Mutable State
data State = State
    { stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    , stateWinClose        :: !Bool
    , stateClock           :: !UTCTime
    --- IO
    , stateXAngle          :: !Double
    , stateYAngle          :: !Double
    , stateZDist           :: !Double
    , stateMouseDown       :: !Bool
    , stateDragging        :: !Bool
    , stateDragStartX      :: !Double
    , stateDragStartY      :: !Double
    , stateDragStartXAngle :: !Double
    , stateDragStartYAngle :: !Double
    , statePositionX       :: !Double
    , statePositionY       :: !Double
    , stateCursorPosX      :: !Double
    , stateCursorPosY      :: !Double
    , stateArrowsPressed   :: !ArrowKeyState
    , stateFrustum         :: !(M44 CFloat)
    --- pointer to bindings for locations inside the compiled shader
    --- mutable because shaders may be changed in the future.
    , shdrVertexIndex      :: !GL.AttribLocation
    , shdrColorIndex       :: !GL.AttribLocation
    , shdrNormalIndex      :: !GL.AttribLocation
    , shdrProjMatIndex     :: !GL.UniformLocation
    , shdrViewMatIndex     :: !GL.UniformLocation
    , shdrModelMatIndex    :: !GL.UniformLocation
    , shdrNormalMatIndex   :: !GL.UniformLocation
    , shdrTessInnerIndex   :: !GL.UniformLocation
    , shdrTessOuterIndex   :: !GL.UniformLocation
    , stateTessellationFactor :: !Int
    --- the map
    , stateMap             :: !GL.BufferObject
    , mapVert              :: !GL.NumArrayIndices
    }

type Pioneers = RWST Env () State IO