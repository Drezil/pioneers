module UI.UIOperations where

import qualified Data.HashMap.Strict as Map

import UI.UIBaseData
import UI.UIClasses

defaultUIState :: UIButtonState
defaultUIState = UIButtonState False False False False False False

--TODO
-- |The function 'isInside' tests whether a point is inside the widget or any child.
--  A screen position may be inside the bounding box of a widget but not considered part of the component.
--  The function returns all hit widgets that have no hit children or 'Nothing' if the point neither hits any
--  component nor the parent widget itself.
isInside :: ScreenUnit -- ^screen x coordinate
         -> ScreenUnit -- ^screen y coordinate
         -> UIId       -- ^the parent widget
         -> [UIId]
isInside x' y' wg =
    case isInsideSelf x' y' wg of -- test inside parent's bounding box
        False -> []
        True -> case concat $ map (isInside x' y') (getChildren wg) of
            [] -> [toGUIAny wg]
            l  -> l
--TODO: Priority queue?
