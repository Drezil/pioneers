module UI.UIOperations where

import           Control.Monad       (liftM)
import qualified Data.HashMap.Strict as Map
import           Data.Maybe

import Types
import UI.UIBaseData
import UI.UIClasses

defaultUIState :: UIButtonState
defaultUIState = UIButtonState False False False False False False
{-# INLINE defaultUIState #-}

toGUIAny :: Map.HashMap UIId (GUIAny m) -> UIId -> GUIAny m
toGUIAny m uid = fromMaybe (error "map does not contain requested key") (Map.lookup uid m)
{-# INLINE toGUIAny #-}

toGUIAnys :: Map.HashMap UIId (GUIAny m) -> [UIId] -> [GUIAny m]
toGUIAnys m = mapMaybe (flip Map.lookup m)
{-# INLINE toGUIAnys #-}
-- TODO: check for missing components?


-- |The function 'getInside' returns child widgets that overlap with a specific
--  screen position.
--  
--  A screen position may be inside the bounding box of a widget but not
--  considered part of the component. The function returns all hit widgets that 
--  have no hit children, which may be the input widget itself,
--  or @[]@ if the point does not hit the widget.
--  
--  This function returns the widgets themselves unlike 'getInsideId'.
getInside :: Map.HashMap UIId (GUIAny Pioneers) -- ^map containing ui widgets
             -> ScreenUnit -- ^screen x coordinate
             -> ScreenUnit -- ^screen y coordinate
             -> GUIAny Pioneers  -- ^the parent widget
             -> Pioneers [GUIAny Pioneers]
getInside hMap x' y' wg = do
  inside <- isInside x' y' wg
  if inside -- test inside parent's bounding box
  then do
       childrenIds <- getChildren wg
       hitChildren <- liftM concat $ mapM (getInside hMap x' y') (toGUIAnys hMap childrenIds)
       case hitChildren of
            [] -> return [wg]
            _ -> return hitChildren
  else return []
--TODO: Priority queue?

-- |The function 'getInsideId' returns child widgets that overlap with a 
--  specific screen position.
--  
--  A screen position may be inside the bounding box of a widget but not
--  considered part of the component. The function returns all hit widgets that 
--  have no hit children, which may be the input widget itself,
--  or @[]@ if the point does not hit the widget.
--  
--  This function returns the 'UIId's of the widgets unlike 'getInside'.
getInsideId :: Map.HashMap UIId (GUIAny Pioneers) -- ^map containing ui widgets
            -> ScreenUnit -- ^screen x coordinate
            -> ScreenUnit -- ^screen y coordinate
            -> UIId  -- ^the parent widget
            -> Pioneers [UIId]
getInsideId hMap x' y' uid = do
  let wg = toGUIAny hMap uid
  inside <- isInside x' y' wg
  if inside -- test inside parent's bounding box
    then do
      childrenIds <- getChildren wg
      hitChildren <- liftM concat $ mapM (getInsideId hMap x' y') childrenIds
      case hitChildren of
           [] -> return [uid]
           _  -> return hitChildren
    else return []
--TODO: Priority queue?


