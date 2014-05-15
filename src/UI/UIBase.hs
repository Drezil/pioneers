{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, DeriveGeneric #-}
-- widget data is separated into several modules to avoid cyclic dependencies with the Type module
-- TODO: exclude UIMouseState constructor from export?
module UI.UIBase where

import           Control.Lens             ((^.), (.~), (%~), (&), ix, mapped, makeLenses)
import           Control.Monad            (liftM)
import           Data.Array
import          Data.Bits                 (xor)
import           Data.Hashable
import qualified Data.HashMap.Strict      as Map
import           Data.Ix                  ()
-- import           Data.Maybe
import           GHC.Generics (Generic)

-- |Unit of screen/window
type ScreenUnit = Int

-- | @x@ and @y@ position on screen. 
type Pixel = (ScreenUnit, ScreenUnit)

-- |Combines two tuples element-wise. Designed for use with 'Pixel'.
merge :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
merge f (x, y) (x', y') = (f x x', f y y')
{-# INLINABLE merge #-}

-- |Maps the over the elements of a tuple. Designed for use with 'Pixel'.
(>:) :: (a -> b) -> (a, a) -> (b, b)
f >: (x, y) = (f x, f y)
{-# INLINABLE (>:) #-}

-- |Adds two numeric tuples component-wise.
(+:) :: (Num a) => (a, a) -> (a, a) -> (a, a)
(+:) = merge (+)
{-# INLINABLE (+:) #-}

-- |Calculates the component-wise difference between two tuples.
(-:) :: (Num a) => (a, a) -> (a, a) -> (a, a)
(-:) = merge (-)
{-# INLINABLE (-:) #-}

-- |Multiplies two numeric tuples component-wise.
(*:) :: (Num a) => (a, a) -> (a, a) -> (a, a)
(*:) = merge (*)
{-# INLINABLE (*:) #-}

infixl 7 *:
infixl 6 +:, -:
infixl 5 >:

-- |Id to reference a specific widget, must be unique.
newtype UIId = UIId Int deriving (Eq, Ord, Bounded, Ix, Hashable, Show, Read)

-- |Mouse buttons processed by the program.
data MouseButton = LeftButton | RightButton | MiddleButton | MouseX1 | MouseX2
    deriving (Eq, Ord, Enum, Ix, Bounded, Generic, Show, Read)

instance Hashable MouseButton where -- TODO: generic deriving creates functions that run forever
    hash = fromEnum
    hashWithSalt salt x = (salt * 16777619)  `xor` hash x

---------------------------
--- widget state
---------------------------
-- |A key to reference a specific type of 'WidgetState'.
data WidgetStateKey = MouseStateKey
    deriving (Eq, Ord, Enum, Ix, Bounded, Generic, Show, Read)
    
instance Hashable WidgetStateKey where -- TODO: generic deriving creates functions that run forever
    hash = fromEnum
    hashWithSalt salt x = (salt * 16777619)  `xor` hash x

-- |Global tracking of mouse actions to determine event handling.
data UIButtonState = UIButtonState
    { _mousePressed :: Int              -- ^amount of currently pressed buttons
    , _mouseCurrentWidget :: Maybe (UIId, Pixel)
      -- ^the current mouse-active widget and its global coordinates.
      --  If @_mousePressed == 0@: widget the mouse is hovering over,
      --  otherwise: widget the first button has been pressed on.
    , _mouseInside :: Bool -- ^@True@ if the mouse is currently within the mouse-active widget
    } deriving (Show, Eq)

-- |The button dependant state of a 'MouseState'.
data MouseButtonState = MouseButtonState
    { _mouseIsDragging      :: Bool -- ^firing if pressed but not confirmed
    , _mouseIsDeferred    :: Bool
      -- ^deferred if e. g. dragging but outside component
    } deriving (Eq, Show)
    
-- |An applied state a widget may take, depending on its usage and event handlers.
data WidgetState = 
    -- |The state of a mouse reactive ui widget. Referenced by 'MouseStateKey'.
    MouseState
        { _mouseStates   :: Array MouseButton MouseButtonState
        , _mouseIsReady  :: Bool -- ^ready if mouse is above component
        , _mousePixel :: Pixel -- ^current local position of the mouse, only updated if widget is the mouse-active component
        }
    deriving (Eq, Show)

---------------------------
--- events
---------------------------

-- |A key to reference a specific 'EventHandler'.
data EventKey = MouseEvent | MouseMotionEvent
    deriving (Eq, Ord, Enum, Ix, Bounded, Generic, Show, Read)
    
instance Hashable EventKey where -- TODO: generic deriving creates functions that run forever
    hash = fromEnum
    hashWithSalt salt x = (salt * 16777619)  `xor` hash x

--- event handlers

-- |A handler to react on certain events.
data EventHandler m = 
    -- |Handler to control the functionality of a 'GUIWidget' on mouse button events. 
    MouseHandler
        {
        -- |The function 'onMousePressed' is called when a button is pressed
        --  while the widget is mouse-active.
        -- 
        --  A widget becomes mouse-active if no other button is currently pressed and the mouse
        --  coordinates are within the widget's extent ('isInside') until no button is pressed any
        --  more.
        _onMousePress :: MouseButton       -- ^the pressed button
                      -> Pixel             -- ^screen position
                      -> GUIWidget m       -- ^widget the event is invoked on
                      -> m (GUIWidget m)   -- ^widget after the event and the possibly altered mouse handler
        ,
        -- |The function 'onMouseReleased' is called when a button is released
        --  while the widget is mouse-active.
        --  
        --  Thus, the mouse is either within the widget or outside while still dragging.
        _onMouseRelease :: MouseButton       -- ^the released button
                        -> Pixel             -- ^screen position
                        -> GUIWidget m       -- ^widget the event is invoked on
                        -> m (GUIWidget m)   -- ^widget after the event and the altered handler
        }
    |
    -- |Handler to control the functionality of a 'GUIWidget' on mouse movement. 
    MouseMotionHandler
        {
        -- |The function 'onMouseMove' is invoked when the mouse is moved inside the
        --  widget's extent ('isInside') while no button is pressed or when the mouse is inside the
        --  widget's extent while another button loses its mouse-active state. Triggered after
        --  '_onMouseEnter'.
        _onMouseMove :: Pixel             -- ^screen position
                     -> GUIWidget m       -- ^widget the event is invoked on
                     -> m (GUIWidget m)   -- ^widget after the event and the altered handler
        ,
        -- |The function 'onMouseMove' is invoked when the mouse enters the
        --  widget's extent ('isInside') or when the mouse is inside the
        --  widget's extent while another button loses its mouse-active state..
        _onMouseEnter :: Pixel           -- ^screen position
                      -> GUIWidget m     -- ^widget the event is invoked on
                      -> m (GUIWidget m) -- ^widget after the event and the altered handler
        ,
        -- |The function 'onMouseLeave' is invoked when the mouse leaves the
        --  widget's extent ('isInside') while no other widget is mouse-active.
        _onMouseLeave :: Pixel           -- ^screen position
                      -> GUIWidget m     -- ^widget the event is invoked on
                      -> m (GUIWidget m) -- ^widget after the event and the altered handler
        }
    deriving ()


---------------------------
--- widgets
---------------------------

-- |A @GUIWidget@ is a visual object the HUD is composed of. 
data GUIWidget m = Widget
    {_baseProperties :: GUIBaseProperties m
    ,_graphics :: GUIGraphics m
    ,_widgetStates :: Map.HashMap WidgetStateKey WidgetState -- TODO? unsave mapping
    ,_eventHandlers :: Map.HashMap EventKey (EventHandler m) -- no guarantee that data match key
    }

-- |Base properties are fundamental settings of any 'GUIWidget'.
--  They mostly control positioning and widget hierarchy.
data GUIBaseProperties m = BaseProperties
    {
    -- |The @_getBoundary@ function gives the outer extents of the @GUIWidget@.
    --  The bounding box wholly contains all children components.
    _boundary :: m (ScreenUnit, ScreenUnit, ScreenUnit, ScreenUnit) -- ^@(x, y, width, height)@ in pixels (screen coordinates)
    ,
    -- |The @_getChildren@ function returns all children associated with this widget.
    --
    --  All children must be wholly inside the parent's bounding box specified by '_boundary'.
    _children :: m [UIId]
    ,
    -- |The function @_isInside@ tests whether a point is inside the widget itself.
    --  A screen position may be inside the bounding box of a widget but not considered part of the
    --  component.
    --  
    --  The default implementations tests if the point is within the rectangle specified by the 
    --  'getBoundary' function.
    _isInside :: GUIWidget m
              -> Pixel  -- ^local coordinates
              -> m Bool
    ,
    -- |The @_getPriority@ function returns the priority score of a @GUIWidget@.
    --  A widget with a high score is more in the front than a low scored widget.
    _priority :: m Int
    ,
    -- |The @_getShorthand@ function returns a descriptive 'String' mainly for debuggin prupose.
    --  The shorthand should be unique for each instance.
    _shorthand :: String
    }



-- |@GUIGraphics@ functions define the look of a 'GUIWidget'.

data GUIGraphics m = Graphics 
    {temp :: m Int}

$(makeLenses ''UIButtonState)
$(makeLenses ''WidgetState)
$(makeLenses ''MouseButtonState)
$(makeLenses ''EventHandler)
$(makeLenses ''GUIWidget)
$(makeLenses ''GUIBaseProperties)
$(makeLenses ''GUIGraphics)

initialButtonState :: MouseButtonState
initialButtonState = MouseButtonState False False
{-# INLINE initialButtonState #-}

-- |Creates a @UIMouseState@ its @_mouseStates@ are valid 'UIMouseStateSingle' for any @MouseButton@
--  provided in the passed list.
initialMouseState :: WidgetState
initialMouseState = MouseState (array (minBound, maxBound) [(i, initialButtonState) | i <- range (minBound, maxBound)])
                               False (0, 0)
{-# INLINE initialMouseState #-}

-- TODO: combined mouse handler

-- TODO? breaks if button array not of sufficient size -- will be avoided by excluding constructor export
-- |Creates a 'MouseHandler' that sets a widget's 'MouseButtonState' properties if present,
--  only fully functional in conjunction with 'setMouseMotionStateActions'.
setMouseStateActions :: (Monad m) => EventHandler m
setMouseStateActions = MouseHandler press' release'
  where 
    -- |Change 'MouseButtonState's '_mouseIsDragging' to @True@.
    press' b _ w =
        return $ w & widgetStates.(ix MouseStateKey).mouseStates.(ix b).mouseIsDragging .~ True

    -- |Change 'MouseButtonState's '_mouseIsDragging' and '_mouseIsDeferred' to @False@.
    release' b _ w =
        return $ w & widgetStates.(ix MouseStateKey).mouseStates.(ix b) %~
                (mouseIsDragging .~ False) . (mouseIsDeferred .~ False)

-- |Creates a 'MouseHandler' that sets a widget's 'WidgetState MouseState' properties if present,
--  only fully functional in conjunction with 'setMouseStateActions'.
setMouseMotionStateActions :: (Monad m) => EventHandler m
setMouseMotionStateActions = MouseMotionHandler move' enter' leave'
  where
    -- |Updates mouse position.
    move' p w = return $ w & widgetStates.(ix MouseStateKey).mousePixel .~ p
    
    -- |Sets '_mouseIsReady' to @True@, changes '_mouseIsDeferred' to '_mouseIsDragging's current
    --  value and sets '_mouseIsDragging' to @False@. 
    enter' p w = return $ w & widgetStates.(ix MouseStateKey)
                    %~ (mouseIsReady .~ True) . (mousePixel .~ p)
                     . (mouseStates.mapped %~ (mouseIsDeferred .~ False)
                         -- following line executed BEFORE above line
                         . (\sState -> sState & mouseIsDragging .~ not (sState ^. mouseIsDeferred)))
   
    
    -- |Sets '_mouseIsReady' to @False@, changes '_mouseIsDragging' to '_mouseIsDeferred's current
    --  value and sets '_mouseIsDeferred' to @False@. 
    leave' p w = return $ w & widgetStates.(ix MouseStateKey)
                    %~ (mouseIsReady .~ False) . (mousePixel .~ p)
                     . (mouseStates.mapped %~ (mouseIsDragging .~ False)
                         -- following line executed BEFORE above line
                         . (\sState -> sState & mouseIsDeferred .~ not (sState ^. mouseIsDragging)))

-- TODO: make only fire if press started within widget                            
-- |Creates a MouseHandler that reacts on mouse clicks.
-- 
--  Does /not/ update 'WidgetState MouseState'!
buttonMouseActions :: (Monad m) => (MouseButton -> GUIWidget m -> Pixel -> m (GUIWidget m)) -- ^action on button press
                                -> EventHandler m
buttonMouseActions a = MouseHandler press' release'
  where 
    press' _ _ = return

    release' b p w = do fire <- (w ^. baseProperties.isInside) w p
                        if fire then a b w p else return w

-- TODO: make only fire if press started within widget
-- |Creates a MouseHandler that reacts on mouse clicks.
-- 
--  Does /not/ update 'WidgetState MouseState'!
buttonSingleMouseActions :: (Monad m) => (GUIWidget m -> Pixel -> m (GUIWidget m)) -- ^action on button press
                                      -> MouseButton -> EventHandler m
buttonSingleMouseActions a btn = MouseHandler press' release'
  where 
    press' _ _ = return

    release' b p w = do fire <- liftM (b == btn &&) $ (w ^. baseProperties.isInside) w p
                        if fire then a w p else return w

emptyGraphics :: (Monad m) => GUIGraphics m
emptyGraphics = Graphics (return 3)

-- |Extracts width and height from a '_boundary' property of a 'GUIBaseProperties'.
extractExtent :: (ScreenUnit, ScreenUnit, ScreenUnit, ScreenUnit) -> (ScreenUnit, ScreenUnit)
extractExtent (_,_,w,h) = (w,h)
{-# INLINABLE extractExtent #-}

-- |Calculates whether a point's value exceed the given width and height.
isInsideExtent :: (ScreenUnit, ScreenUnit) -> Pixel -> Bool
isInsideExtent (w,h) (x',y') = (x' <= w) && (x' >= 0) && (y' <= h) && (y' >= 0)

-- |Calculates whether a point is within a given rectangle.
isInsideRect :: (ScreenUnit, ScreenUnit, ScreenUnit, ScreenUnit) -> Pixel -> Bool
isInsideRect (x,y,w,h) px = isInsideExtent (w, h) $ px -: (x, y)


-- |@GUIBaseProperties@ with a rectangular base that fills the bounds.
rectangularBase :: (Monad m) => (ScreenUnit, ScreenUnit, ScreenUnit, ScreenUnit) -> [UIId] -> Int -> String -> GUIBaseProperties m
rectangularBase bnd chld prio short =
    BaseProperties (return bnd) (return chld)
                   (\w p -> liftM (flip isInsideExtent p . extractExtent) (w ^. baseProperties.boundary)) -- isInside
                   (return prio) short

debugShowWidget' :: (Monad m) => GUIWidget m -> m String
debugShowWidget' (Widget base _ _ handler) = do
    bnd <- base ^. boundary
    chld <- base ^. children
    prio <- base ^. priority
    let short = base ^. shorthand
    return $ concat [short,"| boundary:", show bnd, ", children:", show chld,
                    ",priority:",show prio, maybe "" (const ", with mouse handler") (Map.lookup MouseEvent handler)]
