{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk.OpenGL as GtkGL

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.Gtk.Gdk.EventM as Event
import qualified Data.Array.IArray as A

import Map.Coordinates
import Map.Map

import Data.Maybe (fromMaybe)
import Debug.Trace
import Data.IntSet as IS
import Data.IORef

import Prelude as P
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.IO.Unsafe (unsafePerformIO)
import GHC.Conc.Sync (unsafeIOToSTM)
import Control.Monad.IO.Class (liftIO)

data ProgramState = PS { keysPressed :: IntSet
                       , px          :: GLfloat
                       , py          :: GLfloat
                       , pz          :: GLfloat
                       , heading     :: GLfloat
                       , pitch       :: GLfloat
                       , dx          :: GLfloat
                       , dy          :: GLfloat
                       , dz          :: GLfloat
                       , dheading    :: GLfloat
                       , dpitch      :: GLfloat }
                       deriving (Show)

animationWaitTime = 3   :: Int
canvasWidth = 1024      :: Int
canvasHeight = 768      :: Int
deltaV = 0.10
deltaH = 0.5
deltaP = 0.15

-- TODO: Put render-stuff in render-module 

glTexCoord2f (x,y) = texCoord (TexCoord2 x y :: TexCoord2 GLfloat)
glVertex3f (x,y,z) = vertex (Vertex3 x y z :: Vertex3 GLfloat)
glNormal3f (x,y,z) = normal (Normal3 x y z :: Normal3 GLfloat)


prepareRenderTile :: PlayMap -> ((Int,Int),MapEntry) -> (Vector3 GLfloat, Color3 GLfloat, [Vertex3 GLfloat])
prepareRenderTile m (c@(cx,cz),(_,t)) = 
                        (
                        if even cx then
                                Vector3 (1.5*(fromIntegral cx)) 0.0 (2*(fromIntegral cz))
                        else
                                Vector3 (1.5*(fromIntegral cx)) 0.0 (2*(fromIntegral cz)-1)
                        ,
                        case t of
                                Water -> Color3 0.5 0.5 1 :: Color3 GLfloat 
                                Grass -> Color3 0.3 0.9 0.1 :: Color3 GLfloat 
                                Sand -> Color3 0.9 0.85 0.7 :: Color3 GLfloat 
                                Mountain -> Color3 0.5 0.5 0.5 :: Color3 GLfloat 
                        ,getTileVertices m c)

renderTile :: (Vector3 GLfloat, Color3 GLfloat, [Vertex3 GLfloat]) -> IO ()
renderTile (coord,c,ts) =
        preservingMatrix $ do
                color c
                translate coord
                _ <- renderPrimitive Polygon $ do
                        glNormal3f(0.0,0.0,1.0)
                        mapM vertex ts
                return ()

drawSphere = do
  renderQuadric (QuadricStyle 
                  (Just Smooth)
                  GenerateTextureCoordinates
                  Outside
                  FillStyle)
                (Sphere 1.0 48 48)
                
-- OpenGL polygon-function for drawing stuff.
display :: MVar ProgramState -> PlayMap -> IO ()
display state t =
  let 
     tiles = P.map (prepareRenderTile t) (A.assocs t)
  in
      do
        ps@PS { 
          px       = px
        , py       = py
        , pz       = pz
        , pitch    = pitch
        , heading  = heading }
                <- readMVar state
        loadIdentity
        GL.rotate pitch (Vector3 1.0 0.0 0.0 :: Vector3 GLfloat)
        GL.rotate heading (Vector3 0.0 1.0 0.0 :: Vector3 GLfloat)
        translate (Vector3 (-px) (-py) (-pz)::Vector3 GLfloat)

        position (Light 0) $= Vertex4 0.0 0.0 (20.0) 1.0

        -- Instead of glBegin ... glEnd there is renderPrimitive.
        --trace (show tiles) $ 
        mapM_ renderTile tiles
        return ()

updateCamera :: MVar ProgramState -> IO ()
updateCamera state = do
        ps@PS { dx       = dx
        , dy       = dy
        , dz       = dz
        , px       = px
        , py       = py
        , pz       = pz
        , pitch    = pitch
        , heading  = heading
        , dpitch   = dpitch
        , dheading = dheading }
                <- takeMVar state

        d@((dx,dy,dz),(heading',pitch')) <- 
          if any (/= 0.0) [dx,dy,dz,dpitch,dheading] then
            preservingMatrix $ do
                -- putStrLn $ unwords $ P.map show [dx,dy,dz,dpitch,dheading]
                loadIdentity
 
                -- in direction of current heading and pitch
                rotate (-heading) (Vector3 0.0 1.0 0.0 :: Vector3 GLfloat)
                rotate (-pitch)   (Vector3 1.0 0.0 0.0 :: Vector3 GLfloat)

                -- perform motion
                translate (Vector3 (-dx) (-dy) (-dz))
 
 
                -- get changes in location components
                mat   <- get (matrix Nothing) :: IO (GLmatrix GLfloat)
                comps <- getMatrixComponents ColumnMajor mat
                -- putStrLn $ show $ comps
                let [dx', dy', dz', _] = drop 12 comps
                    (heading', pitch') = (heading + dheading, pitch + dpitch)
                return ((dx',dy',dz'),(heading',pitch'))
          else
            return ((0,0,0),(heading, pitch))
        putMVar state ps { px         = px + dx
                           , py         = py + dy
                           , pz         = pz + dz
                           , pitch      = pitch'
                           , heading    = heading'
                           }

--Adjust size to given dimensions
reconfigure :: Int -> Int -> IO (Int, Int)
reconfigure w h = do
  -- maintain aspect ratio
  let aspectRatio = (fromIntegral canvasWidth) / (fromIntegral canvasHeight)
      (w1, h1)    = (fromIntegral w, (fromIntegral w) / aspectRatio)
      (w2, h2)    = ((fromIntegral h) * aspectRatio, fromIntegral h)
      (w', h')    = if h1 <= fromIntegral h
                      then (floor w1, floor h1)
                      else (floor w2, floor h2)
  reshape $ Just (w', h') 
  return (w', h')

-- Called by reconfigure to fix the OpenGL viewport according to the
-- dimensions of the widget, appropriately.
reshape :: Maybe (Int, Int) -> IO ()
reshape dims = do
  let (width, height) = fromMaybe (canvasWidth, canvasHeight) dims
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
  matrixMode $= Projection
  loadIdentity
  let (w, h) = if width <= height
                then (fromIntegral height, fromIntegral width )
                else (fromIntegral width,  fromIntegral height)
  -- open, aspect-ratio, near-plane, far-plane
  perspective 60.0 (fromIntegral canvasWidth / fromIntegral canvasHeight) 1.0 100.0
  matrixMode $= Modelview 0
  loadIdentity

keyEvent state press = do
  code <- Event.eventHardwareKeycode
  val  <- Event.eventKeyVal
  mods <- Event.eventModifier
  name <- Event.eventKeyName
  liftIO $ do
          ps@PS { keysPressed = kp
                , dx          = dx
                , dy          = dy
                , dz          = dz
                , px          = px
                , py          = py
                , pz          = pz
                , pitch       = pitch
                , heading     = heading
                , dpitch      = dpitch
                , dheading    = dheading }
            <- takeMVar state
          -- Only process the key event if it is not a repeat
          (ps',ret) <- if (fromIntegral code `member` kp && not press) ||
             (fromIntegral code `notMember` kp && press)
             then let 
                      accept a = return (a, True)
                      deny   a = return (a, False)
                in do
                -- keep list of pressed keys up2date
                ps <- if not press
                  then return ps { keysPressed = fromIntegral code `IS.delete` kp }
                  else return ps { keysPressed = fromIntegral code `IS.insert` kp }
                -- putStrLn $ unwords [name , show val, show code, show ps] -- trace (unwords [name , show val]) -- debugging
                -- process keys
                case press of
                  -- on PRESS only
                  True
                    | code ==  9      -> Gtk.mainQuit >> deny ps
                    | code == 26      -> accept $ ps { dz = dz + deltaV }
                    | code == 40      -> accept $ ps { dz = dz - deltaV }
                    | code == 39      -> accept $ ps { dx = dx + deltaV }
                    | code == 41      -> accept $ ps { dx = dx - deltaV }
                    | code == 65      -> accept $ ps { dy = dy - deltaV }
                    | code == 66      -> accept $ ps { dy = dy + deltaV }
                    | code == 25      -> accept $ ps { dheading = dheading - deltaH }
                    | code == 27      -> accept $ ps { dheading = dheading + deltaH }
                    | otherwise       -> deny ps
                  -- on RELEASE only
                  False
                    | code == 26      -> accept $ ps { dz = dz - deltaV }
                    | code == 40      -> accept $ ps { dz = dz + deltaV }
                    | code == 39      -> accept $ ps { dx = dx - deltaV }
                    | code == 41      -> accept $ ps { dx = dx + deltaV }
                    | code == 65      -> accept $ ps { dy = dy + deltaV }
                    | code == 66      -> accept $ ps { dy = dy - deltaV }
                    | code == 25      -> accept $ ps { dheading = dheading + deltaH }
                    | code == 27      -> accept $ ps { dheading = dheading - deltaH }
                    | otherwise       -> deny ps
             else return (ps, False)
          putMVar state ps'
          return ret

main :: IO ()
main = do
  ! terrain <- testmap
  -- create TVar using unsafePerformIO -> currently no other thread -> OK
  state <- newMVar $ PS {    keysPressed = IS.empty
                             , px          = 7.5
                             , py          = 20
                             , pz          = 15
                             , heading     = 0
                             , pitch       = 60
                             , dx          = 0
                             , dy          = 0
                             , dz          = 0
                             , dheading    = 0
                             , dpitch      = 0}
  trace (show terrain) Gtk.initGUI
  -- Initialise the Gtk+ OpenGL extension
  -- (including reading various command line parameters)
  GtkGL.initGL

  -- We need a OpenGL frame buffer configuration to be able to create other
  -- OpenGL objects.
  glconfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA,
                                 GtkGL.GLModeDepth,
                                 GtkGL.GLModeDouble]

  -- Create an OpenGL drawing area widget
  canvas <- GtkGL.glDrawingAreaNew glconfig

  Gtk.widgetSetSizeRequest canvas canvasWidth canvasHeight

  -- Initialise some GL setting just before the canvas first gets shown
  -- (We can't initialise these things earlier since the GL resources that
  -- we are using wouldn't heve been setup yet)
  Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> do
    reconfigure canvasWidth canvasHeight
    materialAmbient   Front $= Color4 0.4 0.4 0.4 1.0
    materialDiffuse   Front $= Color4 0.4 0.4 0.4 1.0
    materialSpecular  Front $= Color4 0.8 0.8 0.8 1.0
    materialShininess Front $= 25.0

    ambient  (Light 0) $= Color4 0.3 0.3 0.3 1.0
    diffuse  (Light 0) $= Color4 1.0 1.0 1.0 1.0
    specular (Light 0) $= Color4 0.8 0.8 0.8 1.0
    lightModelAmbient  $= Color4 0.2 0.2 0.2 1.0

    lighting        $= Enabled 
    light (Light 0) $= Enabled
    depthFunc       $= Just Less

    clearColor $= Color4 0.0 0.0 0.0 0.0
    drawBuffer $= BackBuffers
    colorMaterial $= Just (Front, Diffuse)

    texture Texture2D $= Enabled

    shadeModel $= Smooth
    return ()
    {-clearColor $= (Color4 0.0 0.0 0.0 0.0)
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
    depthFunc $= Just Less
    drawBuffer $= BackBuffers-}
 
  -- Set the repaint handler
  Gtk.onExpose canvas $ \_ -> do
    GtkGL.withGLDrawingArea canvas $ \glwindow -> do
      GL.clear [GL.DepthBuffer, GL.ColorBuffer]
      display state terrain
      GtkGL.glDrawableSwapBuffers glwindow
    return True
 
  -- Setup the animation
  Gtk.timeoutAddFull (do
      updateCamera state
      Gtk.widgetQueueDraw canvas
      return True)
    Gtk.priorityDefaultIdle animationWaitTime
 
  --------------------------------
  -- Setup the rest of the GUI:
  --
  -- Objects
  window <- Gtk.windowNew
  button <- Gtk.buttonNew
  exitButton <- Gtk.buttonNew
  label <- Gtk.labelNew (Just "Gtk2Hs using OpenGL via HOpenGL!")
  vbox <- Gtk.vBoxNew False 4

  --Wrench them together

  Gtk.set window [ Gtk.containerBorderWidth := 10,
                   Gtk.containerChild := canvas,
                   Gtk.windowTitle := "Pioneer" ]

  ------
  -- Events
  --
  Gtk.afterClicked button (putStrLn "Hello World")
  Gtk.afterClicked exitButton Gtk.mainQuit
  Gtk.onDestroy window Gtk.mainQuit

  Gtk.on window Gtk.keyPressEvent $ keyEvent state True
 
  Gtk.on window Gtk.keyReleaseEvent $ keyEvent state False

 -- "reshape" event handler
  Gtk.on canvas Gtk.configureEvent $ Event.tryEvent $ do
    (w, h)   <- Event.eventSize
    (w', h') <- liftIO $ reconfigure w h
    liftIO $ Gtk.labelSetText label $ unwords ["Width:",show w',"Height:",show h']


  Gtk.widgetShowAll window
  Gtk.mainGUI

