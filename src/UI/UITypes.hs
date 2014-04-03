{-# LANGUAGE InstanceSigs, MultiParamTypeClasses, FlexibleInstances #-}

module UI.UITypes where

import Data.List
import Foreign.C                            (CFloat)
import Linear.Matrix (M44)

-- |Unit of screen/window
type ScreenUnit = Int

-- |A viewport to an OpenGL scene.
data Viewport = Viewport
    { _viewportXAngle          :: !Double
    , _viewportYAngle          :: !Double
    , _viewportZDist           :: !Double
    , _viewportFrustum         :: !(M44 CFloat)
    , _viewportPositionX       :: !ScreenUnit -- ^x position in window
    , _viewportPositionY       :: !ScreenUnit -- ^y position in window
    , _viewportWidth           :: !ScreenUnit -- ^viewport width in window
    , _viewportHeight          :: !ScreenUnit -- ^viewport height in window
    } deriving (Eq, Show)
    
data UIState = UIState
    { _uistateIsFiring      :: Bool
    -- ^firing if pressed but not confirmed 
    , _uistateIsFiringAlt   :: Bool
    -- ^firing if pressed but not confirmed (secondary mouse button)
    , _uistateIsDeferred    :: Bool -- ^ deferred if e. g. dragging but outside component
    , _uistateIsDeferredAlt :: Bool
    -- ^deferred if e. g. dragging but outside component (secondary mouse button)
    , _uistateIsReady       :: Bool
    -- ^ready if mouse is above component
    , _uistateIsActivated   :: Bool
    -- ^in activated state (e. g. toggle button)
    } deriving (Eq, Show)


defaultUIState :: UIState
defaultUIState = UIState False False False False False False

class GUIAnyMap w where
    guiAnyMap :: (w -> b) -> GUIAny -> b
    toGUIAny :: w -> GUIAny
    fromGUIAny :: GUIAny -> w
    
    
class (GUIAnyMap uiw) => GUIWidget uiw where
    -- |The 'getBoundary' function gives the outer extents of the 'UIWidget'.
    --  The bounding box wholly contains all children components.
    getBoundary :: uiw -> (ScreenUnit, ScreenUnit, ScreenUnit ,ScreenUnit) -- ^@(x, y, width, height)@ in pixels (screen coordinates)

    -- |The 'getChildren' function returns all children associated with this widget.
    --
    --  All children must be wholly inside the parent's bounding box specified by 'getBoundary'.
    getChildren :: uiw -> [GUIAny]
    getChildren _ = []

    -- |The function 'isInsideSelf' tests whether a point is inside the widget itself.
    --  A screen position may be inside the bounding box of a widget but not considered part of the
    --  component.
    --  
    --  The default implementations tests if the point is within the rectangle specified by the 
    --  'getBoundary' function.
    isInsideSelf :: ScreenUnit -- ^screen x coordinate
                 -> ScreenUnit -- ^screen y coordinate
                 -> uiw       -- ^the parent widget
                 -> Bool
    isInsideSelf x' y' wg = let (x, y, w, h) = getBoundary wg
        in (x' - x <= w) && (x' - x >= 0) && (y' - y <= h) && (y' - y >= 0)

    -- |The function 'isInside' tests whether a point is inside the widget or any child.
    --  A screen position may be inside the bounding box of a widget but not considered part of the component.
    --  The function returns all hit widgets that have no hit children or 'Nothing' if the point neither hits any
    --  component nor the parent widget itself.
    isInside :: ScreenUnit -- ^screen x coordinate
             -> ScreenUnit -- ^screen y coordinate
             -> uiw       -- ^the parent widget
             -> [GUIAny]
    isInside x' y' wg =
        case isInsideSelf x' y' wg of -- test inside parent's bounding box
            False -> []
            True -> case concat $ map (isInside x' y') (getChildren wg) of
                [] -> [toGUIAny wg]
                l  -> l
    --TODO: Priority queue?

    -- |The 'getPriority' function returns the priority score of a 'GUIWidget'.
    --  A widget with a high score is more in the front than a low scored widget.
    getPriority :: uiw -> Int
    getPriority _ = 0
    
    -- |The 'getShorthand' function returns a descriptive 'String' mainly for debuggin prupose.
    --  The shorthand should be unique for each instance.
    getShorthand :: uiw -> String

class MouseHandler a w where
    -- |The function 'onMousePressed' is called when the primary button is pressed
    --  while inside a screen coordinate within the widget ('isInside').
    onMousePressed :: ScreenUnit -- ^screen x coordinate 
                   -> ScreenUnit -- ^screen y coordinate
                   -> w -- ^widget the event is invoked on
                   -> a -> IO (w, a) -- ^widget after the event and the altered handler
    onMousePressed _ _ wg a = return (wg, a)

    -- |The function 'onMouseReleased' is called when the primary button is released
    --  while the pressing event occured within the widget ('isInside').
    --  
    --  Thus, the mouse is either within the widget or outside while still dragging.
    onMouseReleased :: ScreenUnit -- ^screen x coordinate
                    -> ScreenUnit  -- ^screen x coordinate
                    -> w -- ^wdiget the event is invoked on
                    -> a -> IO (w, a) -- ^widget after the event and the altered handler
    onMouseReleased _ _ wg a = return (wg, a)

    -- |The function 'onMousePressed' is called when the secondary button is pressed
    --  while inside a screen coordinate within the widget ('isInside').
    onMousePressedAlt :: ScreenUnit -- ^screen x coordinate 
                   -> ScreenUnit -- ^screen y coordinate
                   -> w -- ^widget the event is invoked on
                   -> a -> IO (w, a) -- ^widget after the event and the altered handler
    onMousePressedAlt _ _ wg a = return (wg, a)

    -- |The function 'onMouseReleased' is called when the secondary button is released
    --  while the pressing event occured within the widget ('isInside').
    --  
    --  Thus, the mouse is either within the widget or outside while still dragging.
    onMouseReleasedAlt :: ScreenUnit -- ^screen x coordinate
                       -> ScreenUnit  -- ^screen x coordinate
                       -> w -- ^wdiget the event is invoked on
                       -> a -> IO (w, a) -- ^widget after the event and the altered handler
    onMouseReleasedAlt _ _ wg a = return (wg, a)
                        
    -- |The function 'onMouseMove' is invoked when the mouse is moved inside the
    --  widget's space ('isInside').
    onMouseMove :: ScreenUnit -- ^screen x coordinate
                -> ScreenUnit -- ^screen y coordinate
                -> w -- ^widget the event is invoked on
                -> a -> IO (w, a) -- ^widget after the event and the altered handler
    onMouseMove _ _ wg a = return (wg, a)
    
    -- |The function 'onMouseMove' is invoked when the mouse enters the
    --  widget's space ('isInside').
    onMouseEnter :: ScreenUnit -- ^screen x coordinate
                 -> ScreenUnit -- ^screen y coordinate
                 -> w -- ^widget the event is invoked on
                 -> a -> IO (w, a) -- ^widget after the event and the altered handler
    onMouseEnter _ _ wg a = return (wg, a)
    
    -- |The function 'onMouseMove' is invoked when the mouse leaves the
    --  widget's space ('isInside').
    onMouseLeave :: ScreenUnit -- ^screen x coordinate
                 -> ScreenUnit -- ^screen y coordinate
                 -> w -- ^widget the event is invoked on
                 -> a -> IO (w, a) -- ^widget after the event and the altered handler
    onMouseLeave _ _ wg a = return (wg, a)

-- |Switches primary and secondary mouse actions.
data MouseHandlerSwitch w h = MouseHandlerSwitch h deriving (Eq, Show)
instance Functor (MouseHandlerSwitch w) where
    fmap :: (h1 -> h2) -> MouseHandlerSwitch w h1 -> MouseHandlerSwitch w h2
    fmap f (MouseHandlerSwitch h) = MouseHandlerSwitch (f h)
instance Monad (MouseHandlerSwitch w) where
    (>>=) :: (MouseHandlerSwitch w h1) -> (h1 -> MouseHandlerSwitch w h2) -> MouseHandlerSwitch w h2 
    (MouseHandlerSwitch h) >>= f = f h
    return :: h -> MouseHandlerSwitch w h
    return h = MouseHandlerSwitch h
instance (MouseHandler h w) => MouseHandler (MouseHandlerSwitch w h) w where
    onMousePressed x y w (MouseHandlerSwitch h) = do
        (w', h') <- onMousePressedAlt x y w h
        return (w', MouseHandlerSwitch h')
    onMouseReleased x y w (MouseHandlerSwitch h) = do
        (w', h') <- onMouseReleasedAlt x y w h
        return (w', MouseHandlerSwitch h')
    onMousePressedAlt x y w (MouseHandlerSwitch h) = do
        (w', h') <- onMousePressed x y w h
        return (w', MouseHandlerSwitch h')
    onMouseReleasedAlt x y w (MouseHandlerSwitch h) = do
        (w', h') <- onMouseReleased x y w h
        return (w', MouseHandlerSwitch h')
    onMouseMove x y w (MouseHandlerSwitch h) = do
        (w', h') <- onMouseMove x y w h
        return (w', MouseHandlerSwitch h')
    onMouseEnter x y w (MouseHandlerSwitch h) = do
        (w', h') <- onMouseEnter x y w h
        return (w', MouseHandlerSwitch h')
    onMouseLeave x y w (MouseHandlerSwitch h) = do
        (w', h') <- onMouseLeave x y w h
        return (w', MouseHandlerSwitch h')


-- !!Important: one handler can only handle one single widget!!
data ButtonHandler w = ButtonHandler 
    { _action :: (w -> ScreenUnit -> ScreenUnit -> IO w)
    , _handlerState :: UIState
    }
instance (Show w) => Show (ButtonHandler w) where
    show (ButtonHandler _ w) = "ButtonHandler [" ++ show w ++ "] " ++ "[action]" 
instance MouseHandler (ButtonHandler w) w where
    -- |Change 'UIState's '_uistateIsFiring' to @True@.
    onMousePressed _ _ wg h@(ButtonHandler _ s) = do
        return (wg, h {_handlerState = s {_uistateIsFiring = True}})

    -- |Change 'UIState's '_uistateIsFiring' to @False@ and
    --  call 'action' if inside the widget or
    --  set '_uistateIsDeferred' to false otherwise.
    onMouseReleased x y wg h@(ButtonHandler f s) = if _uistateIsFiring s 
        then do
            wg' <- f wg x y
            return (wg', h {_handlerState = s {_uistateIsFiring = False}})
        else return (wg, h {_handlerState = s {_uistateIsDeferred = False}})
    
    -- |Do nothing.
    onMouseMove _ _ wg h = return (wg, h)
    
    -- |Set 'UIState's '_uistateIsReady' to @True@ and
    --  update dragging state (only drag if inside widget).
    --  In detail, change 'UIState's '_uistateIsDeferred' to '_uistateIsFiring's current value
    --   and set '_uistateIsFiring' to @False@. 
    onMouseEnter _ _ wg h@(ButtonHandler _ s) = return
        (wg, h {_handlerState = s { _uistateIsFiring = _uistateIsDeferred s
                                  , _uistateIsDeferred = False
                                  , _uistateIsReady = True
                                  }})
    
    -- |Set 'UIState's 'uistateIsReady' to @False@ and
    --  update dragging state (only drag if inside widget).
    --  In detail, change 'UIState's '_uistateIsFiring' to '_uistateIsDeferred's current value
    --  and set '_uistateIsDeferred's' to @False@.
    onMouseLeave _ _ wg h@(ButtonHandler _ s) = return
        (wg, h {_handlerState = s { _uistateIsFiring = False
                                  , _uistateIsDeferred = _uistateIsFiring s
                                  , _uistateIsReady = False
                                  }})


data GUIAny = GUIAnyC GUIContainer
            | GUIAnyP GUIPanel
            | GUIAnyB GUIButton
            deriving (Show)
instance GUIAnyMap GUIAny where
    guiAnyMap f w = f w
    toGUIAny = id
    fromGUIAny = id

instance GUIWidget GUIAny where
    getBoundary (GUIAnyC w) = getBoundary w
    getBoundary (GUIAnyP w) = getBoundary w
    getBoundary (GUIAnyB w) = getBoundary w
    getChildren (GUIAnyC w) = getChildren w
    getChildren (GUIAnyP w) = getChildren w
    getChildren (GUIAnyB w) = getChildren w
    isInsideSelf x y (GUIAnyC w) = (isInsideSelf x y) w
    isInsideSelf x y (GUIAnyP w) = (isInsideSelf x y) w
    isInsideSelf x y (GUIAnyB w) = (isInsideSelf x y) w
    isInside x y (GUIAnyC w) = (isInside x y) w
    isInside x y (GUIAnyP w) = (isInside x y) w
    isInside x y (GUIAnyB w) = (isInside x y) w
    getPriority (GUIAnyC w) = getPriority w
    getPriority (GUIAnyP w) = getPriority w
    getPriority (GUIAnyB w) = getPriority w
    getShorthand (GUIAnyC w) = "A" ++ getShorthand w
    getShorthand (GUIAnyP w) = "A" ++ getShorthand w
    getShorthand (GUIAnyB w) = "A" ++ getShorthand w

-- |A 'GUIContainer' is a widget that may contain additional widgets but does not have a
--  functionality itself.
data GUIContainer = GUIContainer { _screenX :: ScreenUnit, _screenY :: ScreenUnit
                                 , _width :: ScreenUnit, _height :: ScreenUnit
                                 , _children :: [GUIAny]
                                 , _priority :: Int
                                 } deriving (Show)

instance GUIAnyMap GUIContainer where
    guiAnyMap f (GUIAnyC c) = f c
    guiAnyMap _ _ = error "invalid types in guiAnyMap"
    toGUIAny cnt = GUIAnyC cnt
    fromGUIAny (GUIAnyC cnt) = cnt
    fromGUIAny _ = error "invalid GUIAny type"
instance GUIWidget GUIContainer where
    getBoundary :: GUIContainer -> (ScreenUnit, ScreenUnit, ScreenUnit ,ScreenUnit)
    getBoundary cnt = (_screenX cnt, _screenY cnt, _width cnt, _height cnt)
    getChildren cnt = _children cnt
    getPriority cnt = _priority cnt
    getShorthand _ = "CNT"
    
-- |A 'GUIPanel' is much like a 'GUIContainer' but it resizes automatically according to its
--  children components.
data GUIPanel = GUIPanel { _panelContainer :: GUIContainer} deriving (Show)
instance GUIAnyMap GUIPanel where
    guiAnyMap f (GUIAnyP p) = f p
    guiAnyMap _ _ = error "invalid types in guiAnyMap"
    toGUIAny pnl = GUIAnyP pnl
    fromGUIAny (GUIAnyP pnl) = pnl
    fromGUIAny _ = error "invalid GUIAny type"
instance GUIWidget GUIPanel where
    getBoundary pnl = case getChildren $ _panelContainer pnl of
                           [] -> getBoundary $ _panelContainer pnl
                           cs -> foldl1' determineSize $ map getBoundary cs
      where
        determineSize :: (ScreenUnit, ScreenUnit, ScreenUnit ,ScreenUnit) -> (ScreenUnit, ScreenUnit, ScreenUnit ,ScreenUnit) -> (ScreenUnit, ScreenUnit, ScreenUnit ,ScreenUnit)
        determineSize (x, y, w, h) (x', y', w', h') =
            let x'' = if x' < x then x' else x
                y'' = if y' < y then y' else y
                w'' = if x' + w' > x + w then x' + w' - x'' else x + w - x''
                h'' = if y' + h' > y + h then y' + h' - y'' else y + h - y''
            in (x'', y'', w'', h'')
            
    getChildren pnl = getChildren $ _panelContainer pnl
    getPriority pnl = getPriority $ _panelContainer pnl
    getShorthand _ = "PNL"
    
-- |A 'GUIButton' is a dummy datatype for a clickable 'GUIWidget'. Its functinality must be
--  provided by an appropriate 'MouseHanlder'.
data GUIButton = GUIButton { _screenXB :: ScreenUnit, _screenYB :: ScreenUnit
                           , _widthB :: ScreenUnit, _heightB :: ScreenUnit
                           , _priorityB :: Int
                           , _actionB :: (GUIButton -> ScreenUnit -> ScreenUnit -> IO GUIButton)
                           , _handlerStateB :: UIState
                           } deriving ()

instance Show GUIButton where
    show w = "GUIButton {_screenXB = " ++ show (_screenXB w)
                    ++ " _screenYB = " ++ show (_screenYB w)
                    ++ " _widthB = " ++ show (_widthB w)
                    ++ " _heightB = " ++ show (_heightB w)
                    ++ " _priorityB = " ++ show (_screenYB w)
                    ++ " _actionB = " ++ "***"
                    ++ " _handlerStateB = " ++ show (_handlerStateB w)
                    ++ "}"
instance MouseHandler GUIButton GUIButton where
    -- |Change 'UIState's '_uistateIsFiring' to @True@.
    onMousePressed _ _ _ h = let
        h' = h {_handlerStateB = (_handlerStateB h) {_uistateIsFiring = True}}
        in return (h', h')

    -- |Change 'UIState's '_uistateIsFiring' to @False@ and
    --  call '_actionB' if inside the widget or
    --  set '_uistateIsDeferred' to false otherwise.
    onMouseReleased x y wg h =
        if _uistateIsFiring (_handlerStateB h) then do
            wg' <- _actionB h wg x y
            wg'' <- return wg' {_handlerStateB = (_handlerStateB wg') {_uistateIsFiring = False}}
            return (wg'', wg'')
        else let
            wg' = wg {_handlerStateB = (_handlerStateB wg) {_uistateIsDeferred = False}}
            in return (wg', wg')
    
    -- |Do nothing.
    onMouseMove _ _ wg h = return (wg, h)
    
    -- |Set 'UIState's '_uistateIsReady' to @True@ and
    --  update dragging state (only drag if inside widget).
    --  In detail, change 'UIState's '_uistateIsDeferred' to '_uistateIsFiring's current value
    --   and set '_uistateIsFiring' to @False@. 
    onMouseEnter _ _ _ h = let
        s = _handlerStateB h
        h' = h {_handlerStateB = s { _uistateIsFiring = _uistateIsDeferred s
                                    , _uistateIsDeferred = False
                                    , _uistateIsReady = True
                                    }}
        in return (h', h')
    
    -- |Set 'UIState's 'uistateIsReady' to @False@ and
    --  update dragging state (only drag if inside widget).
    --  In detail, change 'UIState's 'uistateIsFiring' to 'uistateIsDeferred's current value
    --  and set 'uistateIsDeferred's' to @False@.
    onMouseLeave _ _ _ h = let
        s = _handlerStateB h
        h' = h {_handlerStateB = s { _uistateIsFiring = False
                                    , _uistateIsDeferred = _uistateIsFiring s
                                    , _uistateIsReady = False
                                    }}
        in return (h', h')
instance GUIAnyMap GUIButton where
    guiAnyMap f (GUIAnyB btn) = f btn
    guiAnyMap _ _ = error "invalid types in guiAnyMap"
    toGUIAny btn = GUIAnyB btn
    fromGUIAny (GUIAnyB btn) = btn
    fromGUIAny _ = error "invalid GUIAny type"
instance GUIWidget GUIButton where
    getBoundary btn = (_screenXB btn, _screenYB btn, _widthB btn, _heightB btn)
    getChildren _ = []
    getPriority btn = _priorityB btn
    getShorthand _ = "BTN"