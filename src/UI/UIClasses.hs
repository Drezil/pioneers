{-# LANGUAGE InstanceSigs, MultiParamTypeClasses, FlexibleInstances #-}

module UI.UIClasses where

import           Control.Lens                         ((^.))
import           Control.Monad
--import           Control.Monad.IO.Class -- MonadIO
import           Control.Monad.RWS.Strict             (get)
import           Data.List
import           Data.Maybe
import qualified Data.HashMap.Strict as Map

import qualified Types as T
import UI.UIBaseData

class GUIAnyMap m w where
    guiAnyMap :: (w -> b) -> GUIAny m -> b
    
class (Monad m) => GUIWidget m uiw where
    -- |The 'getBoundary' function gives the outer extents of the 'UIWidget'.
    --  The bounding box wholly contains all children components.
    getBoundary :: uiw -> m (ScreenUnit, ScreenUnit, ScreenUnit, ScreenUnit) -- ^@(x, y, width, height)@ in pixels (screen coordinates)

    -- |The 'getChildren' function returns all children associated with this widget.
    --
    --  All children must be wholly inside the parent's bounding box specified by 'getBoundary'.
    getChildren :: uiw -> m [UIId]
    getChildren _ = return []

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
    isInsideSelf x' y' wg = do
        (x, y, w, h) <- getBoundary wg
        return $ (x' - x <= w) && (x' - x >= 0) && (y' - y <= h) && (y' - y >= 0)

    -- |The 'getPriority' function returns the priority score of a 'GUIWidget'.
    --  A widget with a high score is more in the front than a low scored widget.
    getPriority :: uiw -> m Int
    getPriority _ = return 0
    
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

class Monad m => MouseHandler a m w where
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

instance (MouseHandler h m w) => MouseHandler (MouseHandlerSwitch h) m w where
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

instance (Monad m, GUIClickable w) => MouseHandler (ButtonHandler m w) m w where
    -- |Change 'UIButtonState's '_buttonstateIsFiring' to @True@.
    onMousePressed _ _ wg h =
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

instance (Monad m) => GUIAnyMap m (GUIAny m) where
    guiAnyMap f w = f w

instance GUIWidget T.Pioneers (GUIAny T.Pioneers) where
    getBoundary (GUIAnyC w) = getBoundary w
    getBoundary (GUIAnyP w) = getBoundary w
    getBoundary (GUIAnyB w _) = getBoundary w
    getChildren (GUIAnyC w) = getChildren w
    getChildren (GUIAnyP w) = getChildren w
    getChildren (GUIAnyB w _) = getChildren w
    isInsideSelf x y (GUIAnyC w) = (isInsideSelf x y) w
    isInsideSelf x y (GUIAnyP w) = (isInsideSelf x y) w
    isInsideSelf x y (GUIAnyB w _) = (isInsideSelf x y) w
    getPriority (GUIAnyC w) = getPriority w
    getPriority (GUIAnyP w) = getPriority w
    getPriority (GUIAnyB w _) = getPriority w
    getShorthand (GUIAnyC w) = do { str <- getShorthand w; return $ "A" ++ str }
    getShorthand (GUIAnyP w) = do { str <- getShorthand w; return $ "A" ++ str }
    getShorthand (GUIAnyB w _) = do { str <- getShorthand w; return $ "A" ++ str }

instance (Monad m) => GUIAnyMap m GUIContainer where
    guiAnyMap f (GUIAnyC c) = f c
    guiAnyMap _ _ = error "invalid types in guiAnyMap"
instance (Monad m) => GUIWidget m GUIContainer where
    getBoundary :: GUIContainer -> m (ScreenUnit, ScreenUnit, ScreenUnit ,ScreenUnit)
    getBoundary cnt = return (_uiScreenX cnt, _uiScreenY cnt, _uiWidth cnt, _uiHeight cnt)
    getChildren cnt = return $ _uiChildren cnt
    getPriority cnt = return $ _uiPriority cnt
    getShorthand _ = return $ "CNT"
    
instance GUIAnyMap m GUIPanel where
    guiAnyMap f (GUIAnyP p) = f p
    guiAnyMap _ _ = error "invalid types in guiAnyMap"
instance GUIWidget T.Pioneers GUIPanel where
    getBoundary pnl = do
        state <- get
        let hmap = state ^. T.ui . T.uiMap
        case _uiChildren $ _panelContainer pnl of
                           [] -> getBoundary $ _panelContainer pnl
                           cs -> do
                                 let widgets = catMaybes $ map (flip Map.lookup hmap) cs
                                 foldl' (liftM2 determineSize) (getBoundary $ _panelContainer pnl) $ map getBoundary widgets
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
    getShorthand _ = return $ "PNL"

instance (Monad m) => GUIAnyMap m GUIButton where
    guiAnyMap f (GUIAnyB btn _) = f btn
    guiAnyMap _ _ = error "invalid types in guiAnyMap"
instance GUIClickable GUIButton where
    getButtonState = _uiButtonState
    updateButtonState f btn = btn {_uiButtonState = f $ _uiButtonState btn}
instance (Monad m) => GUIWidget m GUIButton where
    getBoundary btn = return (_uiScreenXB btn, _uiScreenYB btn, _uiWidthB btn, _uiHeightB btn)
    getChildren _ = return []
    getPriority btn = return $ _uiPriorityB btn
    getShorthand _ = return "BTN"