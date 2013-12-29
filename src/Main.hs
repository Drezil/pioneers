
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk.OpenGL as GtkGL

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.Gtk.Gdk.Events as Event

import Map.Coordinates

import Data.Maybe (fromMaybe)

animationWaitTime = 3
canvasWidth = 640
canvasHeight = 480

-- OpenGL polygon-function for drawing stuff.
display :: IO ()
display = do
  loadIdentity
  -- Instead of glBegin ... glEnd there is renderPrimitive.  
  color (Color3 1 1 1 :: Color3 GLfloat)
  renderPrimitive Polygon $ do
    vertex (Vertex3 0.25 0.25 0.0 :: Vertex3 GLfloat)
    vertex (Vertex3 0.75 0.25 0.0 :: Vertex3 GLfloat)
    vertex (Vertex3 0.75 0.75 0.0 :: Vertex3 GLfloat)
    vertex (Vertex3 0.25 0.75 0.0 :: Vertex3 GLfloat)

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
  perspective 60.0 (fromIntegral canvasWidth / fromIntegral canvasHeight) 1.0 20.0
  matrixMode $= Modelview 0
  loadIdentity

main :: IO ()
main = do
  Gtk.initGUI
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
    clearColor $= (Color4 0.0 0.0 0.0 0.0)
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
    depthFunc $= Just Less
    drawBuffer $= BackBuffers
 
  -- Set the repaint handler
  Gtk.onExpose canvas $ \_ -> do
    GtkGL.withGLDrawingArea canvas $ \glwindow -> do
      GL.clear [GL.DepthBuffer, GL.ColorBuffer]
      display
      GtkGL.glDrawableSwapBuffers glwindow
    return True
 
  -- Setup the animation
  Gtk.timeoutAddFull (do
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
                   Gtk.containerChild := vbox,
                   Gtk.windowTitle := "Pioneer" ]
  Gtk.set button [ Gtk.buttonLabel := "Hello World" ]
  Gtk.set exitButton [ Gtk.buttonLabel := "Quit" ]
  Gtk.set vbox [
                   Gtk.containerChild := canvas,
                   Gtk.containerChild := button,
                   Gtk.containerChild := exitButton,
                   Gtk.containerChild := label
                ]

  Gtk.afterClicked button (putStrLn "Hello World")
  Gtk.afterClicked exitButton Gtk.mainQuit
  Gtk.onDestroy window Gtk.mainQuit

 -- "reshape" event handler
  Gtk.onConfigure canvas $ \ (Event.Configure _ _ _ w h) -> do
    (w', h') <- reconfigure w h
    {- texW   <- Gtk.pixbufGetWidth pb
    texH   <- Gtk.pixbufGetHeight pb
    texBPS <- Gtk.pixbufGetBitsPerSample pb
    texRS  <- Gtk.pixbufGetRowstride pb
    texNCh <- Gtk.pixbufGetNChannels pb-}
    Gtk.labelSetText label $ unwords ["Width:",show w',"Height:",show h'{- ,
                                      "TexW:",show texW,"TexH:",show texH,
                                      "BPS:",show texBPS,"RS:",show texRS,
                                      "NCh:",show texNCh-}]
    return True


  Gtk.widgetShowAll window
  Gtk.mainGUI

