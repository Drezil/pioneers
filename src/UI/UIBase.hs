{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, DeriveGeneric #-}
-- widget data is separated into several modules to avoid cyclic dependencies with the Type module
-- TODO: exclude UIMouseState constructor from export?
module UI.UIBase where

import           Control.Lens             ((^.), (.~), (%~), (&), ix, to, mapped, traverse, makeLenses)
import           Control.Monad            (liftM)
import           Data.Array
import           Data.Hashable
import           Data.Ix                  ()
import           Data.Maybe
import           GHC.Generics (Generic)

-- |Unit of screen/window
type ScreenUnit = Int

-- | @x@ and @y@ position on screen. 
type Pixel = (ScreenUnit, ScreenUnit)

newtype UIId = UIId Int deriving (Eq, Ord, Bounded, Ix, Hashable, Show, Read)

data MouseButton = LeftButton | RightButton | MiddleButton | MouseX1 | MouseX2
    deriving (Eq, Ord, Enum, Ix, Bounded, Generic, Show, Read)

instance Hashable MouseButton

firstButton :: MouseButton
firstButton = LeftButton

lastButton :: MouseButton
lastButton = MiddleButton

-- |The button dependant state of a 'UIMouseState'.
data UIMouseStateSingle = MouseStateSingle
    { _mouseIsFiring      :: Bool -- ^firing if pressed but not confirmed
    , _mouseIsDeferred    :: Bool
      -- ^deferred if e. g. dragging but outside component
    } deriving (Eq, Show)

-- |The state of a clickable ui widget.
data UIMouseState = MouseState
    { _mouseStates :: Array MouseButton UIMouseStateSingle
    , _mouseIsReady       :: Bool -- ^ready if mouse is above component
    } deriving (Eq, Show)


-- |Switches primary and secondary mouse actions.
--  "monad type" "widget type" "original handler"
data MouseHandlerSwitch h = MouseHandlerSwitch h deriving (Eq, Show)

-- |A 'UI.UIClasses.MouseHandler' with button behaviour.
data ButtonHandler m w = ButtonHandler 
    { _action :: w -> Pixel -> m w }
instance Show (ButtonHandler m w) where
  show _ = "ButtonHandler ***"

-- |A @GUIWidget@ is a visual object the HUD is composed of. 
data GUIWidget m = Widget
    {_baseProperties :: GUIBaseProperties m
    ,_mouseActions :: Maybe (GUIMouseActions m)
    ,_graphics :: GUIGraphics m
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
              -> Pixel  -- ^screen position
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

-- |Mouse actions control the functionality of a 'GUIWidget' on mouse events. 
data GUIMouseActions m = MouseActions
    {
    -- |The @_mouseState@ function returns the current mouse state of a widget.
    _mouseState :: UIMouseState
    ,
    -- |The function 'onMousePressed' is called when a button is pressed
    --  while inside a screen coordinate within the widget ('isInside').
    _onMousePress :: MouseButton       -- ^the pressed button
                  -> Pixel             -- ^screen position
                  -> GUIWidget m       -- ^widget the event is invoked on
                  -> m (GUIWidget m)   -- ^widget after the event and the possibly altered mouse handler
    ,
    -- |The function 'onMouseReleased' is called when a button is released
    --  while the pressing event occured within the widget ('isInside').
    --  
    --  Thus, the mouse is either within the widget or outside while still dragging.
    _onMouseRelease :: MouseButton       -- ^the released button
                    -> Pixel             -- ^screen position
                    -> GUIWidget m       -- ^widget the event is invoked on
                    -> m (GUIWidget m)   -- ^widget after the event and the altered handler
    ,
    -- |The function 'onMouseMove' is invoked when the mouse is moved inside the
    --  widget's space ('isInside').
    _onMouseMove :: Pixel             -- ^screen position
                 -> GUIWidget m       -- ^widget the event is invoked on
                 -> m (GUIWidget m)   -- ^widget after the event and the altered handler
    ,
    -- |The function 'onMouseMove' is invoked when the mouse enters the
    --  widget's space ('isInside').
    _onMouseEnter :: Pixel           -- ^screen position
                  -> GUIWidget m     -- ^widget the event is invoked on
                  -> m (GUIWidget m) -- ^widget after the event and the altered handler
    ,
    -- |The function 'onMouseMove' is invoked when the mouse leaves the
    --  widget's space ('isInside').
    _onMouseLeave :: Pixel           -- ^screen position
                  -> GUIWidget m     -- ^widget the event is invoked on
                  -> m (GUIWidget m) -- ^widget after the event and the altered handler
    }


-- |@GUIGraphics@ functions define the look of a 'GUIWidget'.

data GUIGraphics m = Graphics 
    {temp :: m Int}

$(makeLenses ''UIMouseState)
$(makeLenses ''UIMouseStateSingle)
$(makeLenses ''GUIWidget)
$(makeLenses ''GUIBaseProperties)
$(makeLenses ''GUIMouseActions)
$(makeLenses ''GUIGraphics)

initialMouseStateS :: UIMouseStateSingle
initialMouseStateS = MouseStateSingle False False
{-# INLINE initialMouseStateS #-}

-- |Creates a @UIMouseState@ its @_mouseStates@ are valid 'UIMouseStateSingle' for any @MouseButton@
--  provided in the passed list.
initialMouseState :: UIMouseState
initialMouseState = MouseState (array (minBound, maxBound) [(i, initialMouseStateS) | i <- range (minBound, maxBound)])
                               False
{-# INLINE initialMouseState #-}

emptyMouseAction :: (Monad m) => GUIMouseActions m
emptyMouseAction = MouseActions initialMouseState empty'' empty'' empty' empty' empty'
    where empty' _ = return
          empty'' _ _ = return

-- TODO: combined mouse handler

-- TODO? breaks if array not of sufficient size -- will be avoided by excluding constructor export
-- |Creates a @GUIMouseActions@ handler that enables button clicks.
-- 
--  The action is peformed right before the button state change.
buttonMouseActions :: (Monad m) => (MouseButton -> GUIWidget m -> Pixel -> m (GUIWidget m)) -- ^action on button press
                                -> GUIMouseActions m
buttonMouseActions a = MouseActions initialMouseState press' release' move' enter' leave'
  where 
    -- |Change 'UIMouseState's '_mouseIsFiring' to @True@.
    press' b _ w =
        return $ w & mouseActions.traverse.mouseState.mouseStates.(ix b).mouseIsFiring .~ True

    -- |Change 'UIMouseState's '_mouseIsFiring' and '_mouseIsDeferred' to @False@ and
    --  call action if '_mouseIsFiring' was @True@.
    release' b p w =
      let fire = w ^. mouseActions.(to fromJust).mouseState.mouseStates.(to (!b)).mouseIsFiring -- TODO? may break if initialized and called wrongly
      in do w' <- if fire
                  then a b w p
                  else return w
            return $ w' & mouseActions.traverse.mouseState.mouseStates.(ix b) %~
                (mouseIsFiring .~ False) . (mouseIsDeferred .~ False)
    
    -- |Do nothing.
    move' _ = return
    
    -- |Set 'UIMouseState's '_mouseIsReady' to @True@ and
    --  update dragging state (only drag if inside widget).
    --  In detail, change 'UIMouseState's '_mouseIsDeferred' to '_mouseIsFiring's current value
    --  and set '_mouseIsFiring' to @False@. 
    enter' _ w = return $ w & mouseActions.traverse.mouseState %~ (mouseIsReady .~ True)
                        .(mouseStates.mapped %~ (mouseIsDeferred .~ False)
                            -- following line executed BEFORE above line
                            .(\sState -> sState & mouseIsFiring .~ not (sState ^. mouseIsDeferred)))
   
    
    -- |Set 'UIMouseState's 'buttonstateIsReady' to @False@ and
    --  update dragging state (only drag if inside widget).
    --  In detail, change 'UIMouseState's '_buttonstateIsFiring' to '_buttonstateIsDeferred's current value
    --  and set '_buttonstateIsDeferred's' to @False@.
    leave' _ w = return $ w & mouseActions.traverse.mouseState %~ (mouseIsReady .~ False)
                        .(mouseStates.mapped %~ (mouseIsFiring .~ False)
                            -- following line executed BEFORE above line
                            .(\sState -> sState & mouseIsDeferred .~ not (sState ^. mouseIsFiring)))

emptyGraphics :: (Monad m) => GUIGraphics m
emptyGraphics = Graphics (return 3)

isInsideRect :: (ScreenUnit, ScreenUnit, ScreenUnit, ScreenUnit) -> Pixel -> Bool
isInsideRect (x,y,w,h) (x',y') = (x' - x <= w) && (x' - x >= 0) && (y' - y <= h) && (y' - y >= 0)

rectangularBase :: (Monad m) => (ScreenUnit, ScreenUnit, ScreenUnit, ScreenUnit) -> [UIId] -> Int -> String -> GUIBaseProperties m
rectangularBase bnd chld prio short =
    BaseProperties (return bnd) (return chld)
                   (\w p -> liftM (`isInsideRect` p) (w ^. baseProperties.boundary))
                   (return prio) short

debugShowWidget' :: (Monad m) => GUIWidget m -> m String
debugShowWidget' (Widget base mouse _) = do
    bnd <- base ^. boundary
    chld <- base ^. children
    prio <- base ^. priority
    let short = base ^. shorthand
    return $ concat [short,"| boundary:", show bnd, ", children:", show chld,
                    ",priority:",show prio, maybe "" (const ", with mouse handler") mouse]
    
