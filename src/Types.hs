{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Control.Concurrent.STM               (TQueue)
import qualified Graphics.Rendering.OpenGL.GL         as GL
import           Graphics.UI.SDL                      as SDL (Event, Window)
import           Foreign.C                            (CFloat)
import           Data.Time                            (UTCTime)
import Linear.Matrix (M44)
import Control.Monad.RWS.Strict (RWST)
import Control.Lens


--Static Read-Only-State
data Env = Env
    { _eventsChan    :: TQueue Event
    , _windowObject  :: !Window
    , _zDistClosest  :: !Double
    , _zDistFarthest :: !Double
    --, envGLContext     :: !GLContext
    --, envFont          :: TTF.TTFFont
    }

--Mutable State

data Position = Position
    { _x                   :: !Double
    , _y                   :: !Double
    }

data WindowState = WindowState
    { _width               :: !Int
    , _height              :: !Int
    , _shouldClose         :: !Bool
    }

data CameraState = CameraState
    { _xAngle              :: !Double
    , _yAngle              :: !Double
    , _zDist               :: !Double
    , _frustum             :: !(M44 CFloat)
    , _camPosition         :: !Position --TODO: Get rid of cam-prefix
    }

data IOState = IOState
    { _clock               :: !UTCTime
    }

data GameState = GameState
    {
    }

data MouseState = MouseState
    { _isDown              :: !Bool
    , _isDragging          :: !Bool
    , _dragStartX          :: !Double
    , _dragStartY          :: !Double
    , _dragStartXAngle     :: !Double
    , _dragStartYAngle     :: !Double
    , _mousePosition       :: !Position --TODO: Get rid of mouse-prefix
    }

data ArrowKeyState = ArrowKeyState {
         _up      :: !Bool
        ,_down    :: !Bool
        ,_left    :: !Bool
        ,_right   :: !Bool
}

data KeyboardState = KeyboardState
    { _arrowsPressed        :: !ArrowKeyState
    }

data GLMapState = GLMapState
    { _shdrVertexIndex      :: !GL.AttribLocation
    , _shdrColorIndex       :: !GL.AttribLocation
    , _shdrNormalIndex      :: !GL.AttribLocation
    , _shdrProjMatIndex     :: !GL.UniformLocation
    , _shdrViewMatIndex     :: !GL.UniformLocation
    , _shdrModelMatIndex    :: !GL.UniformLocation
    , _shdrNormalMatIndex   :: !GL.UniformLocation
    , _shdrTessInnerIndex   :: !GL.UniformLocation
    , _shdrTessOuterIndex   :: !GL.UniformLocation
    , _stateTessellationFactor :: !Int
    , _stateMap             :: !GL.BufferObject
    , _mapVert              :: !GL.NumArrayIndices
    }

data GLState = GLState
    { _glMap               :: !GLMapState
    }

data State = State
    { _window              :: !WindowState
    , _camera              :: !CameraState
    , _io                  :: !IOState
    , _mouse               :: !MouseState
    , _keyboard            :: !KeyboardState
    , _gl                  :: !GLState
    , _game                :: !GameState
    }

$(makeLenses ''State)
$(makeLenses ''GLState)
$(makeLenses ''GLMapState)
$(makeLenses ''KeyboardState)
$(makeLenses ''ArrowKeyState)
$(makeLenses ''MouseState)
$(makeLenses ''GameState)
$(makeLenses ''IOState)
$(makeLenses ''CameraState)
$(makeLenses ''WindowState)
$(makeLenses ''Position)
$(makeLenses ''Env)


type Pioneers = RWST Env () State IO
