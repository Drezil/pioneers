{-# LANGUAGE ExistentialQuantification #-}

module UI.Callbacks where

import Control.Monad.Trans (liftIO)
import Types
import UI.UITypes

data Pixel = Pixel Int Int

getGUI :: [GUIAny]
getGUI = [ toGUIAny $ GUIContainer 0 0 120 80 [] 1
         , toGUIAny $ GUIPanel $ GUIContainer 0 0 0 0
             [toGUIAny $ GUIContainer  0 80 100 200 [] 4
             ,toGUIAny $GUIButton 50 400 200 175 2 defaultUIState testMessage
             ] 3
         ]

testMessage :: (Show w) => w -> ScreenUnit -> ScreenUnit -> IO w
testMessage w x y = do
    putStrLn ("\tclick on " ++ show x ++ "," ++ show y)
    return w

-- | Handler for UI-Inputs.
--   Indicates a primary click on something (e.g. left-click, touch on Touchpad, fire on Gamepad, ...
clickHandler :: Pixel -> Pioneers ()
clickHandler (Pixel x y) = case concatMap (isInside x y) getGUI of
    [] -> liftIO $ putStrLn $ unwords ["button press on (",show x,",",show y,")"]
    hit -> liftIO $ do
        _ <- sequence $ map (\w ->
            case w of
                 (GUIAnyB b h) -> do
                      putStrLn $ "hitting " ++ getShorthand w ++ ": " ++ show (getBoundary w) ++ " " ++ show (getPriority w)
                          ++ " at ["++show x++","++show y++"]"
                      (b', h') <- onMousePressed x y b h
                      _ <- onMouseReleased x y b' h'
                      return ()
                 _ -> putStrLn $ "hitting " ++ getShorthand w ++ ": " ++ show (getBoundary w) ++ " " ++ show (getPriority w)
                          ++ " at ["++show x++","++show y++"]"
            ) hit
        return ()


-- | Handler for UI-Inputs.
--   Indicates an alternate click on something (e.g. right-click, touch&hold on Touchpad, ...
alternateClickHandler :: Pixel -> Pioneers ()
alternateClickHandler (Pixel x y) = liftIO $ putStrLn $ unwords ["alternate press on (",show x,",",show y,")"]


--TODO: Add scroll-Handler, return (Pioneers Bool) to indicate event-bubbling etc.
--TODO: Maybe queues are better?
