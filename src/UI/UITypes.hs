{-# LANGUAGE InstanceSigs, ExistentialQuantification #-}

module UI.UITypes where


type IntScreen = Int

data GUIAny = forall wg. GUIWidget wg => GUIAny wg

class GUIWidget uiw where
    -- |The 'getBoundary' function gives the outer extents of the 'UIWidget'.
    --  The bounding box wholly contains all children components.
    getBoundary :: uiw -> (IntScreen, IntScreen, IntScreen ,IntScreen) -- ^@(x, y, width, height)@ in pixels (screen coordinates)

    -- |The 'getChildren' function returns all children associated with this widget.
    --
    --  All children must be wholly inside the parent's bounding box specified by 'getBoundary'.
    getChildren :: uiw -> [GUIAny]
    getChildren _ = []

    -- |The function 'isInsideSelf' tests whether a point is inside the widget itself.
    --  A screen position may be inside the bounding box of a widget but not considered part of the component.
    isInsideSelf :: IntScreen -- ^screen x coordinate
                 -> IntScreen -- ^screen y coordinate
                 -> uiw       -- ^the parent widget
                 -> Bool
    isInsideSelf x' y' wg = let (x, y, w, h) = getBoundary wg
        in (x' - x <= w) && (x' - x >= 0) && (y' - y <= h) && (y' - y >= 0)

    -- |The function 'isInside' tests whether a point is inside the widget or any child.
    --  A screen position may be inside the bounding box of a widget but not considered part of the component.
    --  The function returns all hit widgets that have no hit children or 'Nothing' if the point neither hits any
    --  component nor the parent widget itself.
    isInside :: IntScreen -- ^screen x coordinate
             -> IntScreen -- ^screen y coordinate
             -> uiw       -- ^the parent widget
             -> [GUIAny]
    isInside x' y' wg =
        case isInsideSelf x' y' wg of -- test inside parent's bounding box
            False -> []
            True -> case concat $ map (isInside x' y') (getChildren wg) of
                [] -> [GUIAny wg]
                l  -> l
    --TODO: Priority queue?

    -- |The 'getPriority' function returns the priority score of a 'GUIWidget'.
    --  A widget with a high score is more in the front than a low scored widget.
    getPriority :: uiw -> Int
    getPriority _ = 0


instance GUIWidget GUIAny where
    getBoundary (GUIAny wg) = getBoundary wg
    isInsideSelf x y (GUIAny wg) = isInsideSelf x y wg
    isInside x y (GUIAny wg) = isInside x y wg
    getChildren (GUIAny wg) = getChildren wg
    getPriority (GUIAny wg) = getPriority wg

data GUIContainer = GUIContainer {_screenX :: IntScreen, _screenY :: IntScreen
                                 , _width :: IntScreen, _height :: IntScreen
                                 , _children :: [GUIAny]
                                 , _priority :: Int}

instance GUIWidget GUIContainer where
    getBoundary :: GUIContainer -> (IntScreen, IntScreen, IntScreen ,IntScreen)
    getBoundary cnt = (_screenX cnt, _screenY cnt, _width cnt, _height cnt)
    getChildren cnt = _children cnt
    getPriority cnt = _priority cnt