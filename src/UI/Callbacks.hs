module UI.Callbacks where

import Control.Monad.Trans (liftIO)
import Types

data Pixel = Pixel Int Int

-- | Handler for UI-Inputs.
--   Indicates a primary click on something (e.g. left-click, touch on Touchpad, fire on Gamepad, ...
clickHandler :: Pixel -> Pioneers ()
clickHandler (Pixel x y) = liftIO $ putStrLn $ unwords ["button press on (",show x,",",show y,")"]

-- | Handler for UI-Inputs.
--   Indicates an alternate click on something (e.g. right-click, touch&hold on Touchpad, ...
alternateClickHandler :: Pixel -> Pioneers ()
alternateClickHandler (Pixel x y) = liftIO $ putStrLn $ unwords ["alternate press on (",show x,",",show y,")"]


--TODO: Add scroll-Handler, return (Pioneers Bool) to indicate event-bubbling etc.
--TODO: Maybe queues are better?
