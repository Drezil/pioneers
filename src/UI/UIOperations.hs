module UI.UIOperations where

import           Control.Lens                    ((^.))
import           Control.Monad                   (liftM)
import qualified Data.HashMap.Strict             as Map
import           Data.Maybe

import Types
import UI.UIBase

toGUIAny :: Map.HashMap UIId (GUIWidget m) -> UIId -> GUIWidget m
toGUIAny m uid = fromMaybe (error "map does not contain requested key") (Map.lookup uid m)
{-# INLINE toGUIAny #-}

toGUIAnys :: Map.HashMap UIId (GUIWidget m) -> [UIId] -> [GUIWidget m]
toGUIAnys m = mapMaybe (`Map.lookup` m)
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
getInside :: Map.HashMap UIId (GUIWidget Pioneers) -- ^map containing ui widgets
          -> Pixel  -- ^screen position
          -> GUIWidget Pioneers  -- ^the parent widget
          -> Pioneers [GUIWidget Pioneers]
getInside hMap px wg = do
  inside <- (wg ^. baseProperties.isInside) wg px
  if inside -- test inside parent's bounding box
  then do
       childrenIds <- wg ^. baseProperties.children
       hitChildren <- liftM concat $ mapM (getInside hMap px) (toGUIAnys hMap childrenIds)
       case hitChildren of
            [] -> return [wg]
            _  -> return hitChildren
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
getInsideId :: Map.HashMap UIId (GUIWidget Pioneers) -- ^map containing ui widgets
            -> Pixel  -- ^screen position
            -> UIId  -- ^the parent widget
            -> Pioneers [UIId]
getInsideId hMap px uid = do
  let wg = toGUIAny hMap uid
  inside <- (wg ^. baseProperties.isInside) wg px
  if inside -- test inside parent's bounding box
    then do
      childrenIds <- wg ^. baseProperties.children
      hitChildren <- liftM concat $ mapM (getInsideId hMap px) childrenIds
      case hitChildren of
           [] -> return [uid]
           _  -> return hitChildren
    else return []
--TODO: Priority queue?


