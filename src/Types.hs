{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Control.Concurrent.STM               (TQueue, TVar, readTVar, writeTVar, atomically)
import qualified Graphics.Rendering.OpenGL.GL         as GL
import           Graphics.UI.SDL                      as SDL (Event, Window)
import           Foreign.C                            (CFloat)
import qualified Data.HashMap.Strict                  as Map
import           Data.Time                            (UTCTime)
import Linear.Matrix (M44)
import Linear (V3)
import Control.Monad.RWS.Strict (RWST, liftIO, get)
import Control.Monad.Writer.Strict
--import Control.Monad (when)
import Control.Lens
import Graphics.Rendering.OpenGL.GL.Texturing.Objects (TextureObject)
import Render.Types
import System.IO
import Importer.IQM.Types
import UI.UIBase
import Map.Types (PlayMap)

data Coord3D a = Coord3D a a a

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
    { __x                   :: !Double
    , __y                   :: !Double
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
    , _camObject           :: !Camera
    }

data IOState = IOState
    { _clock               :: !UTCTime
    , _tessClockFactor     :: !Double
    , _tessClockTime       :: !UTCTime
    }

data GameState = GameState
    { _currentMap          :: !PlayMap
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

-- | State in which all map-related Data is stored
--
--   The map itself is rendered with mapProgram and the shaders given here directly
--   This does not include any objects on the map - only the map itself
--
--   _mapTextures must contain the following Textures (in this ordering) after initialisation:
--
--     1. Grass
--
--     2. Sand
--
--     3. Water
--
--     4. Stone
--
--     5. Snow
--
--     6. Dirt (blended on grass)


data GLMapState = GLMapState
    { _mapShaderData        :: !MapShaderData
    , _mapObjectShaderData  :: !MapObjectShaderData
    , _stateTessellationFactor :: !Int
    , _stateMap             :: !GL.BufferObject
    , _mapVert              :: !GL.NumArrayIndices
    , _mapProgram           :: !GL.Program
    , _overviewTexture      :: !TextureObject
    , _shadowMapTexture     :: !TextureObject
    , _mapTextures          :: ![TextureObject] --TODO: Fix size on list?
    , _objectProgram        :: !GL.Program
    , _mapObjects           :: ![MapObject]
    , _shadowMapProgram     :: !GL.Program
    }

data MapShaderData = MapShaderData
    { shdrVertexIndex      :: !GL.AttribLocation
    , shdrColorIndex       :: !GL.AttribLocation
    , shdrNormalIndex      :: !GL.AttribLocation
    , shdrProjMatIndex     :: !GL.UniformLocation
    , shdrViewMatIndex     :: !GL.UniformLocation
    , shdrModelMatIndex    :: !GL.UniformLocation
    , shdrNormalMatIndex   :: !GL.UniformLocation
    , shdrTessInnerIndex   :: !GL.UniformLocation
    , shdrTessOuterIndex   :: !GL.UniformLocation
    }

data MapObjectShaderData = MapObjectShaderData
    { shdrMOVertexIndex    :: !GL.AttribLocation
    , shdrMOVertexOffsetIndex :: !GL.UniformLocation
    , shdrMONormalIndex    :: !GL.AttribLocation
    , shdrMOTexIndex       :: !GL.AttribLocation
    , shdrMOProjMatIndex   :: !GL.UniformLocation
    , shdrMOViewMatIndex   :: !GL.UniformLocation
    , shdrMOModelMatIndex  :: !GL.UniformLocation
    , shdrMONormalMatIndex :: !GL.UniformLocation
    , shdrMOTessInnerIndex :: !GL.UniformLocation
    , shdrMOTessOuterIndex :: !GL.UniformLocation
    }




data MapObject = MapObject !IQM !MapCoordinates !MapObjectState

data MapObjectState = MapObjectState ()

type MapCoordinates = V3 CFloat

data GLHud = GLHud
    { _hudTexture               :: !TextureObject       -- ^ HUD-Texture itself
    , _hudTexIndex              :: !GL.UniformLocation  -- ^ Position of Overlay-Texture in Shader
    , _hudBackIndex             :: !GL.UniformLocation  -- ^ Position of Background-Texture in Shader
    , _hudVertexIndex           :: !GL.AttribLocation   -- ^ Position of Vertices in Shader
    , _hudVert                  :: !GL.NumArrayIndices  -- ^ Number of Vertices to draw
    , _hudVBO                   :: !GL.BufferObject     -- ^ Vertex-Buffer-Object
    , _hudEBO                   :: !GL.BufferObject     -- ^ Element-Buffer-Object
    , _hudProgram               :: !GL.Program          -- ^ Program for rendering HUD
    }

data GLState = GLState
    { _glMap               :: !GLMapState
    , _glHud               :: !GLHud
    , _glRenderbuffer      :: !GL.RenderbufferObject
    , _glFramebuffer       :: !GL.FramebufferObject
    }

data UIState = UIState
    { _uiHasChanged        :: !Bool
    , _uiMap               :: Map.HashMap UIId (GUIWidget Pioneers)
    , _uiObserverEvents    :: Map.HashMap EventKey [EventHandler Pioneers]
    , _uiRoots             :: !([UIId])
    , _uiButtonState       :: !UIButtonState
    }

data State = State
    { _window              :: !WindowState
    , _camera              :: TVar CameraState
    , _mapTexture          :: TVar TextureObject
    , _camStack            :: TVar (Map.HashMap UIId (CameraState, TextureObject))
    , _io                  :: !IOState
    , _keyboard            :: !KeyboardState
    , _gl                  :: !GLState
    , _game                :: TVar GameState
    , _ui                  :: !UIState
    }

data Entry = Log {msg::String} deriving Eq

instance Show Entry where
  show (Log s) = s

type Logger = WriterT [Entry] IO Handle

type Pioneers = RWST Env () State IO

-- when using TemplateHaskell order of declaration matters
$(makeLenses ''State)
$(makeLenses ''GLState)
$(makeLenses ''GLMapState)
$(makeLenses ''GLHud)
$(makeLenses ''KeyboardState)
$(makeLenses ''ArrowKeyState)
$(makeLenses ''GameState)
$(makeLenses ''IOState)
$(makeLenses ''CameraState)
$(makeLenses ''WindowState)
$(makeLenses ''Position)
$(makeLenses ''Env)
$(makeLenses ''UIState)

-- helper-functions for types

-- | atomically change gamestate on condition
changeIfGamestate :: (GameState -> Bool) -> (GameState -> GameState) -> Pioneers Bool
changeIfGamestate cond f = do
    state <- get
    liftIO $ atomically $ do
        game' <- readTVar (state ^. game)
        let cond' = cond game'
        when cond' (writeTVar (state ^. game) (f game'))
        return cond'


-- | atomically change gamestate
changeGamestate :: (GameState -> GameState) -> Pioneers ()
changeGamestate s = do
        --forget implied result - is True anyway
        _ <- changeIfGamestate (const True) s
        return ()

