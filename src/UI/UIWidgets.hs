{-# LANGUAGE InstanceSigs, MultiParamTypeClasses, FlexibleInstances #-}

module UI.UIWidgets (module UI.UIWidgets, module UI.UIBase) where

import           Control.Concurrent.STM               (atomically)
import           Control.Concurrent.STM.TVar          (readTVarIO, writeTVar)
import           Control.Lens                         ((^.), (.~), (%~), (&), (^?), at)
import           Control.Monad
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.RWS.Strict             (get, modify)
import           Data.List
import           Data.Maybe
import qualified Data.HashMap.Strict as Map

import Types
import Render.Misc                          (curb)
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
                                    (Map.fromList [(ViewportStateKey, initialViewportState)]) -- widget states
                                    (Map.fromList [(MouseEvent, viewportMouseAction)
                                                  ,(MouseMotionEvent, viewportMouseMotionAction)]) -- event handlers
  where
    updateCamera :: Double -> Double -> Double -> Double -> Double -> Double -> Pioneers ()
    updateCamera xStart' yStart' x y sodxa sodya = do
        state <- get
        cam <- liftIO $ readTVarIO (state ^. camera)
        let myrot = (x - xStart') / 2
            mxrot = (y - yStart') / 2
            newXAngle' = sodxa + mxrot/100
            newXAngle  = curb (pi/12) (0.45*pi) newXAngle'
            newYAngle' = sodya + myrot/100
            newYAngle
                 | newYAngle' > pi    = newYAngle' - 2 * pi
                 | newYAngle' < (-pi) = newYAngle' + 2 * pi
                 | otherwise          = newYAngle'
        
        liftIO $ atomically $
            writeTVar (state ^. camera) $ (xAngle .~ newXAngle) . (yAngle .~ newYAngle) $ cam
  
    viewportMouseAction :: WidgetEventHandler Pioneers
    viewportMouseAction =
        let press btn' (x, y) _ w =
              do if (btn == btn') 
                  then do state <- get
                          cam <- liftIO $ readTVarIO (state ^. camera)
                          let sodxa = cam ^. xAngle
                              sodya = cam ^. yAngle
                          updateCamera (fromIntegral x) (fromIntegral y) (fromIntegral x) (fromIntegral y) sodxa sodya
                          return $ w & widgetStates . at ViewportStateKey .~
                              Just (ViewportState True (fromIntegral x) (fromIntegral y) sodxa sodya)
                  else return w
            release btn' _ _ w = if (btn' == btn)
              then
                -- modify ViewportState to "not dragging" or recreate ViewportState state if not present
                return $ w & widgetStates . at ViewportStateKey %~
                    maybe (Just $ initialViewportState) (\s -> Just (s & isDragging .~ False))
              else return w
        in MouseHandler press release
    
    viewportMouseMotionAction :: WidgetEventHandler Pioneers
    viewportMouseMotionAction =
        let move (x, y) w =
              do let mbPosState = w ^. widgetStates.(at ViewportStateKey)
                 case mbPosState of
                      Just posState ->
                        when (maybe False id (posState ^? isDragging)) $ do
                          let xS = fromJust $ posState ^? dragStartX -- fromJust is safe
                              yS = fromJust $ posState ^? dragStartY -- fromJust is safe
                              sodxa = fromJust $ posState ^? dragAngleX -- fromJust is safe
                              sodya = fromJust $ posState ^? dragAngleY -- fromJust is safe
                          updateCamera xS yS (fromIntegral x) (fromIntegral y) sodxa sodya
                      Nothing -> return ()
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
            
            
            