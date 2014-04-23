{-# LANGUAGE InstanceSigs, MultiParamTypeClasses, FlexibleInstances #-}

module UI.UIClasses where

import Types

class GUIAnyMap w where
    guiAnyMap :: (w -> b) -> GUIAny -> b
    
class (GUIAnyMap uiw) => GUIWidget m uiw where
    -- |The 'getBoundary' function gives the outer extents of the 'UIWidget'.
    --  The bounding box wholly contains all children components.
    getBoundary :: uiw -> m (ScreenUnit, ScreenUnit, ScreenUnit ,ScreenUnit) -- ^@(x, y, width, height)@ in pixels (screen coordinates)

    -- |The 'getChildren' function returns all children associated with this widget.
    --
    --  All children must be wholly inside the parent's bounding box specified by 'getBoundary'.
    getChildren :: uiw -> m [UIId]
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
                 -> m Bool
    isInsideSelf x' y' wg = let (x, y, w, h) = getBoundary wg
        in (x' - x <= w) && (x' - x >= 0) && (y' - y <= h) && (y' - y >= 0)

    -- |The 'getPriority' function returns the priority score of a 'GUIWidget'.
    --  A widget with a high score is more in the front than a low scored widget.
    getPriority :: uiw -> m Int
    getPriority _ = 0
    
    -- |The 'getShorthand' function returns a descriptive 'String' mainly for debuggin prupose.
    --  The shorthand should be unique for each instance.
    getShorthand :: uiw -> m String

-- |A 'GUIClickable' represents a widget with a 'UIButtonState'.
-- 
--  Minimal complete definition: 'getButtonState' and either 'updateButtonState' or 'setButtonState'.
class GUIClickable w where
    updateButtonState :: (UIButtonState -> UIButtonState) -> w -> w
    updateButtonState f w = setButtonState (f $ getButtonState w) w
    setButtonState :: UIButtonState -> w -> w
    setButtonState s = updateButtonState (\_ -> s)
    getButtonState :: w -> UIButtonState

class MouseHandler a m w where
    -- |The function 'onMousePressed' is called when the primary button is pressed
    --  while inside a screen coordinate within the widget ('isInside').
    onMousePressed :: ScreenUnit -- ^screen x coordinate 
                   -> ScreenUnit -- ^screen y coordinate
                   -> w -- ^widget the event is invoked on
                   -> a -> m (w, a) -- ^widget after the event and the altered handler
    onMousePressed _ _ wg a = return (wg, a)

    -- |The function 'onMouseReleased' is called when the primary button is released
    --  while the pressing event occured within the widget ('isInside').
    --  
    --  Thus, the mouse is either within the widget or outside while still dragging.
    onMouseReleased :: ScreenUnit -- ^screen x coordinate
                    -> ScreenUnit  -- ^screen x coordinate
                    -> w -- ^wdiget the event is invoked on
                    -> a -> m (w, a) -- ^widget after the event and the altered handler
    onMouseReleased _ _ wg a = return (wg, a)

    -- |The function 'onMousePressed' is called when the secondary button is pressed
    --  while inside a screen coordinate within the widget ('isInside').
    onMousePressedAlt :: ScreenUnit -- ^screen x coordinate 
                   -> ScreenUnit -- ^screen y coordinate
                   -> w -- ^widget the event is invoked on
                   -> a -> m (w, a) -- ^widget after the event and the altered handler
    onMousePressedAlt _ _ wg a = return (wg, a)

    -- |The function 'onMouseReleased' is called when the secondary button is released
    --  while the pressing event occured within the widget ('isInside').
    --  
    --  Thus, the mouse is either within the widget or outside while still dragging.
    onMouseReleasedAlt :: ScreenUnit -- ^screen x coordinate
                       -> ScreenUnit  -- ^screen x coordinate
                       -> w -- ^wdiget the event is invoked on
                       -> a -> m (w, a) -- ^widget after the event and the altered handler
    onMouseReleasedAlt _ _ wg a = return (wg, a)
                        
    -- |The function 'onMouseMove' is invoked when the mouse is moved inside the
    --  widget's space ('isInside').
    onMouseMove :: ScreenUnit -- ^screen x coordinate
                -> ScreenUnit -- ^screen y coordinate
                -> w -- ^widget the event is invoked on
                -> a -> m (w, a) -- ^widget after the event and the altered handler
    onMouseMove _ _ wg a = return (wg, a)
    
    -- |The function 'onMouseMove' is invoked when the mouse enters the
    --  widget's space ('isInside').
    onMouseEnter :: ScreenUnit -- ^screen x coordinate
                 -> ScreenUnit -- ^screen y coordinate
                 -> w -- ^widget the event is invoked on
                 -> a -> m (w, a) -- ^widget after the event and the altered handler
    onMouseEnter _ _ wg a = return (wg, a)
    
    -- |The function 'onMouseMove' is invoked when the mouse leaves the
    --  widget's space ('isInside').
    onMouseLeave :: ScreenUnit -- ^screen x coordinate
                 -> ScreenUnit -- ^screen y coordinate
                 -> w -- ^widget the event is invoked on
                 -> a -> m (w, a) -- ^widget after the event and the altered handler
    onMouseLeave _ _ wg a = return (wg, a)

instance (MouseHandler h m w) => MouseHandler (MouseHandlerSwitch w h) w where
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

instance (GUIClickable w) => MouseHandler (ButtonHandler m w) w where
    -- |Change 'UIButtonState's '_buttonstateIsFiring' to @True@.
    onMousePressed _ _ wg h = do
        return (updateButtonState (\s -> s {_buttonstateIsFiring = True}) wg, h)

    -- |Change 'UIButtonState's '_buttonstateIsFiring' to @False@ and
    --  call 'action' if inside the widget or
    --  set '_buttonstateIsDeferred' to false otherwise.
    onMouseReleased x y wg h@(ButtonHandler action) = if _buttonstateIsFiring $ getButtonState wg 
        then do
            wg' <- action wg x y
            return (updateButtonState (\s -> s {_buttonstateIsFiring = False}) wg', h)
        else return (updateButtonState (\s -> s {_buttonstateIsDeferred = False}) wg, h)
    
    -- |Do nothing.
    onMouseMove _ _ wg h = return (wg, h)
    
    -- |Set 'UIButtonState's '_buttonstateIsReady' to @True@ and
    --  update dragging state (only drag if inside widget).
    --  In detail, change 'UIButtonState's '_buttonstateIsDeferred' to '_buttonstateIsFiring's current value
    --   and set '_buttonstateIsFiring' to @False@. 
    onMouseEnter _ _ wg h = return
        (updateButtonState (\s -> s { _buttonstateIsFiring = _buttonstateIsDeferred s
                                    , _buttonstateIsDeferred = False
                                    , _buttonstateIsReady = True
                                    }) wg
                                    , h)
    
    -- |Set 'UIButtonState's 'buttonstateIsReady' to @False@ and
    --  update dragging state (only drag if inside widget).
    --  In detail, change 'UIButtonState's '_buttonstateIsFiring' to '_buttonstateIsDeferred's current value
    --  and set '_buttonstateIsDeferred's' to @False@.
    onMouseLeave _ _ wg h = return
        (updateButtonState (\s -> s { _buttonstateIsFiring = False
                                    , _buttonstateIsDeferred = _buttonstateIsFiring s
                                    , _buttonstateIsReady = False
                                    }) wg
                                    , h)

instance GUIAnyMap (GUIAny m) where
    guiAnyMap f w = f w

instance GUIWidget m (GUIAny m) where
    getBoundary (GUIAnyC w) = getBoundary w
    getBoundary (GUIAnyP w) = getBoundary w
    getBoundary (GUIAnyB w _) = getBoundary w
    getChildren (GUIAnyC w) = getChildren w
    getChildren (GUIAnyP w) = getChildren w
    getChildren (GUIAnyB w _) = getChildren w
    isInsideSelf x y (GUIAnyC w) = (isInsideSelf x y) w
    isInsideSelf x y (GUIAnyP w) = (isInsideSelf x y) w
    isInsideSelf x y (GUIAnyB w _) = (isInsideSelf x y) w
    isInside x y (GUIAnyC w) = (isInside x y) w
    isInside x y (GUIAnyP w) = (isInside x y) w
    isInside x y (GUIAnyB w _) = (isInside x y) w
    getPriority (GUIAnyC w) = getPriority w
    getPriority (GUIAnyP w) = getPriority w
    getPriority (GUIAnyB w _) = getPriority w
    getShorthand (GUIAnyC w) = "A" ++ getShorthand w
    getShorthand (GUIAnyP w) = "A" ++ getShorthand w
    getShorthand (GUIAnyB w _) = "A" ++ getShorthand w

instance GUIAnyMap GUIContainer where
    guiAnyMap f (GUIAnyC c) = f c
    guiAnyMap _ _ = error "invalid types in guiAnyMap"
instance GUIWidget m GUIContainer where
    getBoundary :: GUIContainer -> m (ScreenUnit, ScreenUnit, ScreenUnit ,ScreenUnit)
    getBoundary cnt = return (_screenX cnt, _screenY cnt, _width cnt, _height cnt)
    getChildren cnt = return $ _children cnt
    getPriority cnt = return $ _priority cnt
    getShorthand _ = return $ "CNT"
    
-- |A 'GUIPanel' is much like a 'GUIContainer' but it resizes automatically according to its
--  children components.
data GUIPanel = GUIPanel { _panelContainer :: GUIContainer} deriving (Show)
instance GUIAnyMap GUIPanel where
    guiAnyMap f (GUIAnyP p) = f p
    guiAnyMap _ _ = error "invalid types in guiAnyMap"
instance GUIWidget m GUIPanel where
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
            
    getChildren pnl = return $ getChildren $ _panelContainer pnl
    getPriority pnl = return $ getPriority $ _panelContainer pnl
    getShorthand _ = return $ "PNL"

instance GUIAnyMap GUIButton where
    guiAnyMap f (GUIAnyB btn _) = f btn
    guiAnyMap _ _ = error "invalid types in guiAnyMap"
instance GUIClickable GUIButton where
    getButtonState = _buttonState
    updateButtonState f btn = btn {_buttonState = f $ _buttonState btn}
instance GUIWidget m GUIButton where
    getBoundary btn = return (_screenXB btn, _screenYB btn, _widthB btn, _heightB btn)
    getChildren _ = return []
    getPriority btn = return $ _priorityB btn
    getShorthand _ = return "BTN"