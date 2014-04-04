{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Control.Concurrent.STM               (TQueue)
import qualified Graphics.Rendering.OpenGL.GL         as GL
import           Graphics.UI.SDL                      as SDL (Event, Window, Texture, Renderer)
import           Foreign.C                            (CFloat)
import           Data.Time                            (UTCTime)
import Linear.Matrix (M44)
import Control.Monad.RWS.Strict (RWST)
import Control.Lens
import Graphics.Rendering.OpenGL.GL.Texturing.Objects (TextureObject)


--Static Read-Only-State
data Env = Env
    { _eventsChan    :: TQueue Event
    , _windowObject  :: !Window
    , _zDistClosest  :: !Double
    , _zDistFarthest :: !Double
    --, envGLContext     :: !GLContext
    --, envFont          :: TTF.TTFFont
--    , _renderer      :: !Renderer
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
    , _mapProgram           :: !GL.Program
    }

data GLHud = GLHud
    { _hudTexture               :: !TextureObject       -- ^ Texture itself
    , _hudTexIndex              :: !GL.UniformLocation  -- ^ Position of Texture in Shader
    , _hudVertexIndex           :: !GL.AttribLocation   -- ^ Position of Vertices in Shader
    , _hudVert                  :: !GL.NumArrayIndices  -- ^ Number of Vertices to draw
    , _hudVBO                   :: !GL.BufferObject     -- ^ Vertex-Buffer-Object
    , _hudEBO                   :: !GL.BufferObject     -- ^ Element-Buffer-Object
    , _hudProgram               :: !GL.Program          -- ^ Program for rendering HUD
    }

data GLState = GLState
    { _glMap               :: !GLMapState
    , _glHud               :: !GLHud
    }

data UIState = UIState
    {
    }

data State = State
    { _window              :: !WindowState
    , _camera              :: !CameraState
    , _io                  :: !IOState
    , _mouse               :: !MouseState
    , _keyboard            :: !KeyboardState
    , _gl                  :: !GLState
    , _game                :: !GameState
    , _ui                  :: !UIState
    }

$(makeLenses ''State)
$(makeLenses ''GLState)
$(makeLenses ''GLMapState)
$(makeLenses ''GLHud)
$(makeLenses ''KeyboardState)
$(makeLenses ''ArrowKeyState)
$(makeLenses ''MouseState)
$(makeLenses ''GameState)
$(makeLenses ''IOState)
$(makeLenses ''CameraState)
$(makeLenses ''WindowState)
$(makeLenses ''Position)
$(makeLenses ''Env)
$(makeLenses ''UIState)


type Pioneers = RWST Env () State IO
