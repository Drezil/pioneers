{-# LANGUAGE ExistentialQuantification #-}

module UI.Callbacks where

import Control.Monad.Trans (liftIO)
import Types
import UI.UITypes

data Pixel = Pixel Int Int

getGUI :: [GUIAny]
getGUI = (GUIAny $ GUIContainer 0 0 120 80 [] 1):(GUIAny $ GUIContainer 50 60 300 700 [(GUIAny $ GUIContainer 55 65 200 400 [] 5)] 1):[]

-- | Handler for UI-Inputs.
--   Indicates a primary click on something (e.g. left-click, touch on Touchpad, fire on Gamepad, ...
clickHandler :: Pixel -> Pioneers ()
clickHandler (Pixel x y) = case concat $ map (isInside x y) getGUI of
    [] -> liftIO $ putStrLn $ unwords ["button press on (",show x,",",show y,")"]
    hit -> liftIO $ putStrLn $ unwords $ foldl (++) ["hitting"] ([map (\w -> (show.getBoundary) w ++ ' ':(show.getPriority) w) hit])

-- | Handler for UI-Inputs.
--   Indicates an alternate click on something (e.g. right-click, touch&hold on Touchpad, ...
alternateClickHandler :: Pixel -> Pioneers ()
alternateClickHandler (Pixel x y) = liftIO $ putStrLn $ unwords ["alternate press on (",show x,",",show y,")"]


--TODO: Add scroll-Handler, return (Pioneers Bool) to indicate event-bubbling etc.
--TODO: Maybe queues are better?
