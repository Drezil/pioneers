module UI.UIOperations where

import           Control.Lens                    ((^.))
import           Control.Monad                   (liftM)
--import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.RWS.Strict        (get)
import qualified Data.HashMap.Strict             as Map
import           Data.Maybe

import Types
import UI.UIBase

-- TODO: test GUI function to scan for overlapping widgets

toGUIAny :: Map.HashMap UIId (GUIWidget m) -> UIId -> GUIWidget m -- TODO: what to do if widget not inside map -> inconsistent state
toGUIAny m uid = fromMaybe (error "map does not contain requested key") (Map.lookup uid m)
{-# INLINABLE toGUIAny #-}

toGUIAnys :: Map.HashMap UIId (GUIWidget m) -> [UIId] -> [GUIWidget m]
toGUIAnys m = mapMaybe (`Map.lookup` m)
{-# INLINABLE toGUIAnys #-}
-- TODO: check for missing components?

-- | Tests whether a point is inside a widget by testing its bounding box first.
isInsideFast :: Monad m => GUIWidget m
             -> Pixel  -- ^ local coordinates
             -> m Bool
isInsideFast wg px = do
  (_, _, w, h) <- wg ^. baseProperties.boundary
  liftM (isInsideExtent (w, h) px &&) $ (wg ^. baseProperties.isInside) wg px


-- |The function 'getInsideId' returns child widgets that overlap with a 
--  specific screen position and the pixel's local coordinates.
--  
--  A screen position may be inside the bounding box of a widget but not
--  considered part of the component. The function returns all hit widgets that 
--  have no hit children, which may be the input widget itself,
--  or @[]@ if the point does not hit the widget.
getInsideId :: Pixel  -- ^parent’s local coordinates
            -> UIId  -- ^the parent widget
            -> Pioneers [(UIId, Pixel)]
getInsideId px uid = do
  state <- get
  let wg  = toGUIAny (state ^. ui.uiMap) uid
  (bX, bY, _, _) <- wg ^. baseProperties.boundary
  let px' = px -: (bX, bY)
  inside <- isInsideFast wg px'
  if inside -- test inside parent's bounding box
    then do
      childrenIds <- wg ^. baseProperties.children
      hitChildren <- liftM concat $ mapM (getInsideId px') childrenIds
      case hitChildren of
           [] -> return [(uid, px')]
           _  -> return hitChildren
    else return []
--TODO: Priority queue?
--TODO: only needs to return single target if non-overlapping-child convention applies

-- TODO not needed if non-overlapping-child convention applies
getLeadingWidget :: [(UIId, Pixel)]  -- ^widgets and their screen positions
                 -> Pioneers (Maybe (UIId, Pixel))    -- ^the leading widget
getLeadingWidget [] = return Nothing
getLeadingWidget (x:_) = return $ Just x
              
-- |The function 'isHittingChild' tests if a pixel is hitting a child of the given widget.
--  
--  @'Left' 'False'@ is returned if the point is outside the widget,
--  @'Left' 'True'@ is returned if the point is inside the widget and hits no child and
--  'Right' in combination with both the innermost hit child and the position’s local coordinates
--  is returned otherwise.
isHittingChild :: Pixel -- ^parent’s local coordinates
               -> GUIWidget Pioneers -- ^parent widget
               -> Pioneers (Either Bool (UIId, Pixel))
isHittingChild px wg = do
  isIn <- isInsideFast wg px
  if isIn
    then do
      chld <- wg ^. baseProperties.children
      hitChld <- liftM concat $ mapM (getInsideId px) chld
      hitLead <- getLeadingWidget hitChld
      case hitLead of
           Nothing -> return $ Left True
           Just h -> return $ Right h
    else return $ Left False