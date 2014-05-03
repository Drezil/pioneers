module UI.UIOperations where

import           Control.Lens                    ((^.))
import           Control.Monad                   (liftM)
import qualified Data.HashMap.Strict             as Map
import           Data.Maybe

import Types
import UI.UIBase

-- TODO: test GUI function to scan for overlapping widgets

toGUIAny :: Map.HashMap UIId (GUIWidget m) -> UIId -> GUIWidget m
toGUIAny m uid = fromMaybe (error "map does not contain requested key") (Map.lookup uid m)
{-# INLINABLE toGUIAny #-}

toGUIAnys :: Map.HashMap UIId (GUIWidget m) -> [UIId] -> [GUIWidget m]
toGUIAnys m = mapMaybe (`Map.lookup` m)
{-# INLINABLE toGUIAnys #-}
-- TODO: check for missing components?


-- |The function 'getInsideId' returns child widgets that overlap with a 
--  specific screen position.
--  
--  A screen position may be inside the bounding box of a widget but not
--  considered part of the component. The function returns all hit widgets that 
--  have no hit children, which may be the input widget itself,
--  or @[]@ if the point does not hit the widget.
getInsideId :: Map.HashMap UIId (GUIWidget Pioneers) -- ^map containing ui widgets
            -> Pixel  -- ^screen position
            -> UIId  -- ^the parent widget
            -> Pioneers [UIId]
getInsideId hMap px uid = do
  let wg = toGUIAny hMap uid
  bnd@(bX, bY, _, _) <- wg ^. baseProperties.boundary
  inside <- liftM (isInsideRect bnd px &&) $ (wg ^. baseProperties.isInside) wg px
  if inside -- test inside parent's bounding box
    then do
      childrenIds <- wg ^. baseProperties.children
      hitChildren <- liftM concat $ mapM (getInsideId hMap (px -: (bX, bY))) childrenIds
      case hitChildren of
           [] -> return [uid]
           _  -> return hitChildren
    else return []
--TODO: Priority queue?


