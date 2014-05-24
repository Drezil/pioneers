{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, DeriveGeneric, KindSignatures #-}
-- widget data is separated into several modules to avoid cyclic dependencies with the Type module
-- TODO: exclude UIMouseState constructor from export?
module UI.UIBase where

import           Control.Lens             ((^.), (.~), (%~), (&), ix, mapped, makeLenses)
import           Control.Monad            (join,liftM)
import           Data.Array
import           Data.Bits                 (xor)
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

-- |Maps over the elements of a tuple. Designed for use with 'Pixel'.
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
    
-- |An applied state a widget may take, depending on its usage and event handlers. Corresponding Key: 'WidgetStateKey'. 
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

-- |A key to reference a specific 'WidgetEventHandler'.
data WidgetEventKey = MouseEvent | MouseMotionEvent
    deriving (Eq, Ord, Enum, Ix, Bounded, Generic, Show, Read)
    
instance Hashable WidgetEventKey where -- TODO: generic deriving creates functions that run forever
    hash = fromEnum
    hashWithSalt salt x = (salt * 16777619)  `xor` hash x

--- event handlers

-- |A handler to react on certain events. Corresponding key: 'WidgetEventKey'.
data WidgetEventHandler m = 
    -- |Handler to control the functionality of a 'GUIWidget' on mouse button events.
    --  
    --  All screen coordinates are widget-local coordinates.
    MouseHandler
        {
        -- |The function 'onMousePressed' is called when a button is pressed
        --  while the button is mouse-active.
        --  
        --  The boolean value indicates if the button press happened within the widget
        --  ('_isInside').
        --  
        --  The function returns the altered widget resulting from the button press.
        _onMousePress :: MouseButton -> Pixel -> Bool -> GUIWidget m -> m (GUIWidget m)
        ,
        -- |The function 'onMouseReleased' is called when a button is released
        --  while the widget is mouse-active.
        --  
        --  Thus, the mouse is either within the widget or outside while still dragging.
        --  
        --  
        --  The boolean value indicates if the button release happened within the widget
        --  ('_isInside').
        --  
        --  The function returns the altered widget resulting from the button press.
        _onMouseRelease :: MouseButton -> Pixel -> Bool -> GUIWidget m -> m (GUIWidget m)
        }
    |
    -- |Handler to control the functionality of a 'GUIWidget' on mouse movement.
    --  
    --  All screen coordinates are widget-local coordinates.
    MouseMotionHandler
        {
        -- |The function 'onMouseMove' is invoked when the mouse is moved inside the
        --  widget’s extent ('isInside') while no button is pressed or when the mouse is inside the
        --  widget’s extent while another button loses its mouse-active state. Triggered after
        --  '_onMouseEnter' or '_onMouseLeave' (only if still mouse-active on leaving) if applicable.
        --  
        -- The function returns the altered widget resulting from the button press.
        _onMouseMove :: Pixel -> GUIWidget m -> m (GUIWidget m)
        ,
        -- |The function 'onMouseMove' is invoked when the mouse enters the
        --  widget’s extent ('isInside') or when the mouse is inside the
        --  widget’s extent while another button loses its mouse-active state.
        --  
        -- The function returns the altered widget resulting from the button press.
        _onMouseEnter :: Pixel -> GUIWidget m -> m (GUIWidget m)
        ,
        -- |The function 'onMouseLeave' is invoked when the mouse leaves the
        --  widget’s extent ('isInside') while no other widget is mouse-active.
        --  
        -- The function returns the altered widget resulting from the button press.
        _onMouseLeave :: Pixel -> GUIWidget m -> m (GUIWidget m)
        }
    deriving ()

-- |A key to reference a specific 'EventHandler'.
data EventKey = WindowEvent | WidgetPositionEvent 
    deriving (Eq, Ord, Enum, Ix, Bounded, Generic, Show, Read)

instance Hashable EventKey where -- TODO: generic deriving creates functions that run forever
    hash = fromEnum
    hashWithSalt salt x = (salt * 16777619)  `xor` hash x

 -- |A handler to react on certain events. Corresponding key: 'EventKey'.
data EventHandler (m :: * -> *) = 
    WindowHandler
        {
        -- |The function '_onWindowResize' is invoked when the global application window changes size.
        --  
        --  The input is the window’s new width and height in that order.
        --  
        --  The returned handler is resulting handler that may change by the event. Its type must
        --  remain @WindowHandler@. 
        _onWindowResize :: ScreenUnit -> ScreenUnit -> m (EventHandler m)
        ,
        -- |Unique id to identify an event instance.
        _eventId :: UIId
        }
    
instance Eq (EventHandler m) where
    WindowHandler _ id' == WindowHandler _ id'' = id' == id''
    _ == _ = False


---------------------------
--- widgets
---------------------------

-- |A @GUIWidget@ is a visual object the HUD is composed of. 
data GUIWidget m = Widget
    {_baseProperties :: GUIBaseProperties m
    ,_graphics :: GUIGraphics m
    ,_widgetStates :: Map.HashMap WidgetStateKey WidgetState -- TODO? unsave mapping
    ,_eventHandlers :: Map.HashMap WidgetEventKey (WidgetEventHandler m) -- no guarantee that data match key
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
    --  All children must be wholly inside the parent’s bounding box specified by '_boundary'.
    _children :: m [UIId]
    ,
    -- |The function @_isInside@ tests whether a point is inside the widget itself.
    --  A screen position may be inside the bounding box of a widget but not considered part of the
    --  component.
    --  
    --  The default implementations tests if the point is within the rectangle specified by the 
    --  'getBoundary' function.
    --  
    --  The passed coordinates are widget-local coordinates.
    _isInside :: GUIWidget m -> Pixel -> m Bool
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

data GUIGraphics (m :: * -> *) = Graphics

$(makeLenses ''UIButtonState)
$(makeLenses ''WidgetState)
$(makeLenses ''MouseButtonState)
$(makeLenses ''WidgetEventHandler)
$(makeLenses ''GUIWidget)
$(makeLenses ''GUIBaseProperties)
$(makeLenses ''GUIGraphics)

-- |Creates a default @MouseButtonState@.
initialButtonState :: MouseButtonState
initialButtonState = MouseButtonState False False
{-# INLINE initialButtonState #-}

-- |Creates a 'MouseState' its @_mouseStates@ are valid 'MouseButtonState's for any 'MouseButton'.
initialMouseState :: WidgetState
initialMouseState = MouseState (array (minBound, maxBound) [(i, initialButtonState) | i <- range (minBound, maxBound)])
                               False (0, 0)
{-# INLINE initialMouseState #-}

-- |The function 'combinedMouseHandler' creates a 'MouseHandler' by composing the action functions
--  of two handlers. Thereby, the resulting widget of the first handler is the input widget of the
--  second handler and all other parameters are the same for both function calls.
--  
--  If not both input handlers are of type @MouseHandler@ an error is raised.
combinedMouseHandler :: (Monad m) => WidgetEventHandler m -> WidgetEventHandler m -> WidgetEventHandler m
combinedMouseHandler (MouseHandler p1 r1) (MouseHandler p2 r2) =
    MouseHandler (comb p1 p2) (comb r1 r2)
  where comb h1 h2 btn px inside = join . liftM (h2 btn px inside) . h1 btn px inside
combinedMouseHandler _ _ = error $ "combineMouseHandler can only combine two WidgetEventHandler" ++
    " with constructor MouseHandler"

-- |The function 'combinedMouseMotionHandler' creates a 'MouseHandler' by composing the action
--  functions of two handlers. Thereby, the resulting widget of the second handler is the input
--  widget of the second handler and all other parameters are the same for both function calls.
--  
--  If not both input handlers are of type @MouseMotionHandler@ an error is raised.
combinedMouseMotionHandler :: (Monad m) => WidgetEventHandler m -> WidgetEventHandler m -> WidgetEventHandler m
combinedMouseMotionHandler (MouseMotionHandler m1 e1 l1) (MouseMotionHandler m2 e2 l2) =
    MouseMotionHandler (comb m1 m2) (comb e1 e2) (comb l1 l2)
  where comb h1 h2 px = join . liftM (h2 px) . h1 px
combinedMouseMotionHandler _ _ = error $ "combineMouseMotionHandler can only combine two WidgetEventHandler" ++
    " with constructor MouseMotionHandler" 

-- |The function 'emptyMouseHandler' creates a 'MouseHandler' that does nothing.
--  It may be useful as construction kit.
--  
--  >>> emptyMouseHandler & _onMousePress .~ myPressFunction
--  >>> emptyMouseHandler { _onMousePress = myPressFunction }
emptyMouseHandler :: (Monad m) => WidgetEventHandler m
emptyMouseHandler = MouseHandler (\_ _ _ -> return) (\_ _ _ -> return)

-- |The function 'emptyMouseMotionHandler' creates a 'MouseMotionHandler' that does nothing.
--  It may be useful as construction kit.
--  
--  >>> emptyMouseMotionHandler & _onMouseMove .~ myMoveFunction
--  >>> emptyMouseHandler { _onMouseMove = myMoveFunction }
emptyMouseMotionHandler :: (Monad m) => WidgetEventHandler m
emptyMouseMotionHandler = MouseMotionHandler (const return) (const return) (const return)

-- TODO? breaks if button array not of sufficient size -- will be avoided by excluding constructor export
-- |Creates a 'MouseHandler' that sets a widget’s 'MouseButtonState' properties if present,
--  only fully functional in conjunction with 'setMouseMotionStateActions'.
setMouseStateActions :: (Monad m) => WidgetEventHandler m
setMouseStateActions = MouseHandler press' release'
  where 
    -- |Change 'MouseButtonState'’s '_mouseIsDragging' to @True@.
    press' b _ _ w =
        return $ w & widgetStates.(ix MouseStateKey).mouseStates.(ix b).mouseIsDragging .~ True

    -- |Change 'MouseButtonState'’s '_mouseIsDragging' and '_mouseIsDeferred' to @False@.
    release' b _ _ w =
        return $ w & widgetStates.(ix MouseStateKey).mouseStates.(ix b) %~
                (mouseIsDragging .~ False) . (mouseIsDeferred .~ False)

-- |Creates a 'MouseHandler' that sets a widget’s 'MouseState' properties if present,
--  only fully functional in conjunction with 'setMouseStateActions'.
setMouseMotionStateActions :: (Monad m) => WidgetEventHandler m
setMouseMotionStateActions = MouseMotionHandler move' enter' leave'
  where
    -- |Updates mouse position.
    move' p w = return $ w & widgetStates.(ix MouseStateKey).mousePixel .~ p
    
    -- |Sets '_mouseIsReady' to @True@, changes '_mouseIsDeferred' to '_mouseIsDragging'’s current
    --  value and sets '_mouseIsDragging' to @False@. 
    enter' p w = return $ w & widgetStates.(ix MouseStateKey)
                    %~ (mouseIsReady .~ True) . (mousePixel .~ p)
                     . (mouseStates.mapped %~ (mouseIsDeferred .~ False)
                         -- following line executed BEFORE above line
                         . (\sState -> sState & mouseIsDragging .~ not (sState ^. mouseIsDeferred)))
   
    
    -- |Sets '_mouseIsReady' to @False@, changes '_mouseIsDragging' to '_mouseIsDeferred'’s current
    --  value and sets '_mouseIsDeferred' to @False@. 
    leave' p w = return $ w & widgetStates.(ix MouseStateKey)
                    %~ (mouseIsReady .~ False) . (mousePixel .~ p)
                     . (mouseStates.mapped %~ (mouseIsDragging .~ False)
                         -- following line executed BEFORE above line
                         . (\sState -> sState & mouseIsDeferred .~ not (sState ^. mouseIsDragging)))

-- TODO: make only fire if press started within widget                            
-- |Creates a 'MouseHandler' that reacts on mouse clicks.
-- 
--  Does /not/ update the widget’s 'MouseState'!
buttonMouseActions :: (Monad m) => (MouseButton -> GUIWidget m -> Pixel -> m (GUIWidget m)) -- ^action on button press
                                -> WidgetEventHandler m
buttonMouseActions a = MouseHandler press' release'
  where 
    press' _ _ _ = return

    release' b p inside w = if inside then a b w p else return w

-- TODO: make only fire if press started within widget
-- |Creates a 'MouseHandler' that reacts on mouse clicks.
-- 
--  Does /not/ update the widget’s 'MouseState'!
buttonSingleMouseActions :: (Monad m) => (GUIWidget m -> Pixel -> m (GUIWidget m)) -- ^action on button press
                                      -> MouseButton -> WidgetEventHandler m
buttonSingleMouseActions a btn = MouseHandler press' release'
  where 
    press' _ _ _ = return

    release' b p inside w = if inside && b == btn then a w p else return w

emptyGraphics :: (Monad m) => GUIGraphics m
emptyGraphics = Graphics

-- |Extracts width and height from a '_boundary' property of a 'GUIBaseProperties'.
extractExtent :: (ScreenUnit, ScreenUnit, ScreenUnit, ScreenUnit) -> (ScreenUnit, ScreenUnit)
extractExtent (_,_,w,h) = (w,h)
{-# INLINABLE extractExtent #-}

-- |Calculates whether a point’s value exceed the given width and height.
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
