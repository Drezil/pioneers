{-# LANGUAGE InstanceSigs, MultiParamTypeClasses, FlexibleInstances #-}

module UI.UIBaseData where

import Data.Hashable
import Data.List
import Foreign.C                            (CFloat)
import Linear.Matrix (M44)

-- |Unit of screen/window
type ScreenUnit = Int


newtype UIId = Int deriving (Eq,Ord,Show,Read,Bounded,Ix,Hashable)

-- |The state of a clickable ui widget.
data UIButtonState = UIButtonState
    { _buttonstateIsFiring      :: Bool
    -- ^firing if pressed but not confirmed 
    , _buttonstateIsFiringAlt   :: Bool
    -- ^firing if pressed but not confirmed (secondary mouse button)
    , _buttonstateIsDeferred    :: Bool -- ^ deferred if e. g. dragging but outside component
    , _buttonstateIsDeferredAlt :: Bool
    -- ^deferred if e. g. dragging but outside component (secondary mouse button)
    , _buttonstateIsReady       :: Bool
    -- ^ready if mouse is above component
    , _buttonstateIsActivated   :: Bool
    -- ^in activated state (e. g. toggle button)
    } deriving (Eq, Show)


-- |Switches primary and secondary mouse actions.
--  "monad type" "widget type" "original handler"
data MouseHandlerSwitch w h = MouseHandlerSwitch h deriving (Eq, Show)

-- |A 'UI.UIClasses.MouseHandler' with button behaviour.
data ButtonHandler m w = ButtonHandler 
    { _action :: (w -> ScreenUnit -> ScreenUnit -> m w) }
instance Show (ButtonHandler w) where
    show _ = "ButtonHandler ***"

-- |A collection data type that may hold any usable ui element. @m@ is a monad.
data GUIAny m = GUIAnyC GUIContainer
              | GUIAnyP GUIPanel
              | GUIAnyB GUIButton (ButtonHandler m GUIButton)
              deriving (Show)


-- |A 'GUIContainer' is a widget that may contain additional widgets but does not have a
--  functionality itself.
data GUIContainer = GUIContainer { _screenX :: ScreenUnit, _screenY :: ScreenUnit
                                 , _width :: ScreenUnit, _height :: ScreenUnit
                                 , _children :: [UIId]
                                 , _priority :: Int
                                 } deriving (Show)

-- |A 'GUIPanel' is much like a 'GUIContainer' but it resizes automatically according to its
--  children components.
data GUIPanel = GUIPanel { _panelContainer :: GUIContainer} deriving (Show)
    
-- |A 'GUIButton' is a clickable 'GUIWidget'. Its functinality must be
--  provided by an appropriate 'MouseHanlder'.
data GUIButton = GUIButton { _screenXB :: ScreenUnit, _screenYB :: ScreenUnit
                           , _widthB :: ScreenUnit, _heightB :: ScreenUnit
                           , _priorityB :: Int
                           , _buttonState :: UIButtonState
                           } deriving ()
instance Show GUIButton where
    show w = "GUIButton {_screenXB = " ++ show (_screenXB w)
                    ++ " _screenYB = " ++ show (_screenYB w)
                    ++ " _widthB = " ++ show (_widthB w)
                    ++ " _heightB = " ++ show (_heightB w)
                    ++ " _priorityB = " ++ show (_screenYB w)
                    ++ " _buttonState = " ++ show (_buttonState w)
                    ++ "}"
