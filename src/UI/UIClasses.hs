{-# LANGUAGE InstanceSigs, MultiParamTypeClasses, FlexibleInstances #-}

module UI.UIClasses (module UI.UIClasses, module UI.UIBaseData) where

import           Control.Lens                         ((^.), (.~), (&))
import           Control.Monad
--import           Control.Monad.IO.Class -- MonadIO
import           Control.Monad.RWS.Strict             (get)
import           Data.List
import           Data.Maybe
import qualified Data.HashMap.Strict as Map

import           Types
import UI.UIBaseData


createContainer :: (Monad m) => (ScreenUnit, ScreenUnit, ScreenUnit, ScreenUnit) -> [UIId] -> Int -> GUIWidget m
createContainer bnd chld prio = Widget (rectangularBase bnd chld prio "CNT")
                                          Nothing
                                          emptyGraphics


createPanel :: (ScreenUnit, ScreenUnit, ScreenUnit, ScreenUnit) -> [UIId] -> Int -> GUIWidget Pioneers
createPanel bnd chld prio = Widget (rectangularBase bnd chld prio "PNL" & boundary .~ autosize')
                                      Nothing
                                      emptyGraphics
  where
    autosize' :: Pioneers (ScreenUnit, ScreenUnit, ScreenUnit, ScreenUnit)
    autosize' = do
        state <- get
        let hmap = state ^. ui . uiMap
            -- TODO: local coordinates
            determineSize' :: (ScreenUnit, ScreenUnit, ScreenUnit ,ScreenUnit) -> (ScreenUnit, ScreenUnit, ScreenUnit ,ScreenUnit) -> (ScreenUnit, ScreenUnit, ScreenUnit ,ScreenUnit)
            determineSize' (x, y, w, h) (x', y', w', h') =
               let x'' = if x' < x then x' else x
                   y'' = if y' < y then y' else y
                   w'' = if x' + w' > x + w then x' + w' - x'' else x + w - x''
                   h'' = if y' + h' > y + h then y' + h' - y'' else y + h - y''
                in (x'', y'', w'', h'')
        case chld of
             [] -> return bnd
             cs -> do let widgets = mapMaybe (`Map.lookup` hmap) cs
                      foldl' (liftM2 determineSize') (return bnd) $ map (\w -> w ^. baseProperties.boundary) widgets

createButton :: (Monad m) => (ScreenUnit, ScreenUnit, ScreenUnit, ScreenUnit) -> Int -> (MouseButton -> GUIWidget m -> Pixel -> m (GUIWidget m)) -> GUIWidget m
createButton bnd prio action = Widget (rectangularBase bnd [] prio "BTN")
                                         (Just $ buttonMouseActions action)
                                         emptyGraphics
