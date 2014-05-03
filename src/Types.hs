{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Control.Concurrent.STM               (TQueue)
import qualified Graphics.Rendering.OpenGL.GL         as GL
import           Graphics.UI.SDL                      as SDL (Event, Window)
import           Foreign.C                            (CFloat)
import qualified Data.HashMap.Strict                  as Map
import           Data.Time                            (UTCTime)
import Linear.Matrix (M44)
import Control.Monad.RWS.Strict (RWST)
import Control.Lens
import Graphics.Rendering.OpenGL.GL.Texturing.Objects (TextureObject)
import Render.Types
import UI.UIBase


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
    , _renderedMapTexture   :: !TextureObject --TODO: Probably move to UI?
    , _overviewTexture      :: !TextureObject
    , _mapTextures          :: ![TextureObject] --TODO: Fix size on list?
    }

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
    , _uiRoots             :: [UIId]
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

type Pioneers = RWST Env () State IO

-- when using TemplateHaskell order of declaration matters
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

data Structure = Flag           -- Flag
               | Woodcutter     -- Huts
               | Forester
               | Stonemason
               | Fisher
               | Hunter
               | Barracks
               | Guardhouse
               | LookoutTower
               | Well
               | Sawmill        -- Houses
               | Slaughterhouse
               | Mill
               | Bakery
               | IronSmelter
               | Metalworks
               | Armory
               | Mint
               | Shipyard
               | Brewery
               | Storehouse
               | Watchtower
               | Catapult
               | GoldMine       -- Mines
               | IronMine
               | GraniteMine
               | CoalMine
               | Farm           -- Castles
               | PigFarm
               | DonkeyBreeder
               | Harbor
               | Fortress
               deriving (Show, Eq)

data Amount    = Infinite   -- Neverending supply
               | Finite Int -- Finite supply

-- Extremely preliminary, expand when needed
data Commodity = WoodPlank
               | Sword
               | Fish
               deriving Eq

data Resource  = Coal
               | Iron
               | Gold
               | Granite
               | Water
               | Fishes
               deriving (Show, Eq)

instance Show Amount where
    show (Infinite) = "inexhaustable supply"
    show (Finite n) = show n ++ " left"

instance Show Commodity where
    show WoodPlank = "wooden plank"
    show Sword     = "sword"
    show Fish      = "fish"

