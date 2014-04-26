{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- data and classes are separated into several modules to avoid cyclic dependencies with the Type module

module UI.UIBaseData where

import Data.Hashable
import Data.Ix

-- |Unit of screen/window
type ScreenUnit = Int


newtype UIId = UIId Int deriving (Eq,Ord,Show,Read,Bounded,Ix,Hashable)

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
data MouseHandlerSwitch h = MouseHandlerSwitch h deriving (Eq, Show)

-- |A 'UI.UIClasses.MouseHandler' with button behaviour.
data ButtonHandler m w = ButtonHandler 
    { _action :: (w -> ScreenUnit -> ScreenUnit -> m w) }
instance Show (ButtonHandler m w) where
  show _ = "ButtonHandler ***"

-- |A collection data type that may hold any usable ui element. @m@ is a monad.
data GUIAny m = GUIAnyC GUIContainer
              | GUIAnyP GUIPanel
              | GUIAnyB GUIButton (ButtonHandler m GUIButton)
              deriving (Show)


-- |A 'GUIContainer' is a widget that may contain additional widgets but does not have a
--  functionality itself.
data GUIContainer = GUIContainer { _uiScreenX :: ScreenUnit, _uiScreenY :: ScreenUnit
                                 , _uiWidth :: ScreenUnit, _uiHeight :: ScreenUnit
                                 , _uiChildren :: [UIId]
                                 , _uiPriority :: Int
                                 } deriving (Show)

-- |A 'GUIPanel' is much like a 'GUIContainer' but it resizes automatically according to its
--  children components.
data GUIPanel = GUIPanel { _panelContainer :: GUIContainer} deriving (Show)
    
-- |A 'GUIButton' is a clickable 'GUIWidget'. Its functinality must be
--  provided by an appropriate 'MouseHanlder'.
data GUIButton = GUIButton { _uiScreenXB :: ScreenUnit, _uiScreenYB :: ScreenUnit
                           , _uiWidthB :: ScreenUnit, _uiHeightB :: ScreenUnit
                           , _uiPriorityB :: Int
                           , _uiButtonState :: UIButtonState
                           } deriving ()
instance Show GUIButton where
  show w = "GUIButton {_screenXB = " ++ show (_uiScreenXB w)
        ++ " _screenYB = " ++ show (_uiScreenYB w)
        ++ " _widthB = " ++ show (_uiWidthB w)
        ++ " _heightB = " ++ show (_uiHeightB w)
        ++ " _priorityB = " ++ show (_uiScreenYB w)
        ++ " _buttonState = " ++ show (_uiButtonState w)
        ++ "}"
