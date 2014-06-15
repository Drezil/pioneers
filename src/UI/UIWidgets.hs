{-# LANGUAGE InstanceSigs, MultiParamTypeClasses, FlexibleInstances #-}

module UI.UIWidgets (module UI.UIWidgets, module UI.UIBase) where

import           Control.Concurrent.STM.TVar          (readTVarIO)
import           Control.Lens                         ((^.), (.~), (%~), (&))
import           Control.Monad
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.RWS.Strict             (get, modify)
import           Data.List
import           Data.Maybe
import qualified Data.HashMap.Strict as Map

import           Types
import UI.UIBase
import UI.UIOperations


createContainer :: (Monad m) => (ScreenUnit, ScreenUnit, ScreenUnit, ScreenUnit) -> [UIId] -> Int -> GUIWidget m
createContainer bnd chld prio = Widget (rectangularBase bnd chld prio "CNT")
                                       emptyGraphics
                                       Map.empty -- widget states
                                       Map.empty -- event handlers


createPanel :: (ScreenUnit, ScreenUnit, ScreenUnit, ScreenUnit) -> [UIId] -> Int -> GUIWidget Pioneers
createPanel bnd chld prio = Widget (rectangularBase bnd chld prio "PNL" & boundary .~ autosize')
                                   emptyGraphics
                                   Map.empty -- widget states
                                   Map.empty -- event handlers
  where
    autosize' :: Pioneers (ScreenUnit, ScreenUnit, ScreenUnit, ScreenUnit)
    autosize' = do
        state <- get
        let hmap = state ^. ui . uiMap
            determineSize' :: (ScreenUnit, ScreenUnit, ScreenUnit ,ScreenUnit) -> (ScreenUnit, ScreenUnit, ScreenUnit ,ScreenUnit) -> (ScreenUnit, ScreenUnit, ScreenUnit ,ScreenUnit)
            determineSize' (x, y, w, h) (x', y', w', h') = (x, y, max w (x' + w'), max h (y' + h'))
        case chld of
             [] -> return bnd
             cs -> do let widgets = mapMaybe (`Map.lookup` hmap) cs 
                      foldl' (liftM2 determineSize') (return bnd) $ map (\w -> w ^. baseProperties.boundary) widgets

createButton :: (Monad m) => (ScreenUnit, ScreenUnit, ScreenUnit, ScreenUnit) -> Int -> (MouseButton -> GUIWidget m -> Pixel -> m (GUIWidget m)) -> GUIWidget m
createButton bnd prio action = Widget (rectangularBase bnd [] prio "BTN")
                                      emptyGraphics
                                      (Map.fromList [(MouseStateKey, initialMouseState)]) -- widget states
                                      (Map.fromList [(MouseEvent, buttonMouseActions action)]) -- event handlers

createViewport :: MouseButton -- ^ button to drag with
               -> (ScreenUnit, ScreenUnit, ScreenUnit, ScreenUnit) -> [UIId] -> Int -> GUIWidget Pioneers
createViewport btn bnd chld prio = Widget (rectangularBase bnd chld prio "VWP")
                                    emptyGraphics
                                    Map.empty -- widget states
                                    (Map.fromList [(MouseEvent, viewportMouseAction)
                                                  ,(MouseMotionEvent, viewportMouseMotionAction)]) -- event handlers
  where
    viewportMouseAction :: WidgetEventHandler Pioneers
    viewportMouseAction =
        let press btn' (x, y) _ w =
              do when (btn == btn') $ do
                     state <- get
                     cam <- liftIO $ readTVarIO (state ^. camera)
                     modify $ mouse %~ (isDragging .~ True)
                                     . (dragStartX .~ fromIntegral x)
                                     . (dragStartY .~ fromIntegral y)
                                     . (dragStartXAngle .~ (cam ^. xAngle))
                                     . (dragStartYAngle .~ (cam ^. yAngle))
                                     . (mousePosition.Types._x .~ fromIntegral x)
                                     . (mousePosition.Types._y .~ fromIntegral y)
                 return w
            release btn' _ _ w = do when (btn == btn') (modify $ mouse.isDragging .~ False)
                                    return w
        in MouseHandler press release
    
    viewportMouseMotionAction :: WidgetEventHandler Pioneers
    viewportMouseMotionAction =
        let move (x, y) w =
              do state <- get
                 when (state ^. mouse.isDragging) $
                        modify $ mouse %~ (mousePosition.Types._x .~ fromIntegral x)
                                        . (mousePosition.Types._y .~ fromIntegral y)
                 return w
        in emptyMouseMotionHandler & onMouseMove .~ move
        
resizeToScreenHandler :: UIId -- ^id of a widget
                      -> EventHandler Pioneers
resizeToScreenHandler id' = WindowHandler resize (UIId 0) -- TODO: unique id
  where resize :: ScreenUnit -> ScreenUnit -> Pioneers (EventHandler Pioneers)
        resize w h = do
            state <- get
            let wg = toGUIAny (state ^. ui.uiMap) id'
            (x, y, _, _) <- wg ^. baseProperties.boundary
            let wg' = wg & baseProperties.boundary .~ return (x, y, w-x, h-y)
            modify $ ui.uiMap %~ Map.insert id' wg'
            return $ WindowHandler resize (UIId 0)
            
            
            