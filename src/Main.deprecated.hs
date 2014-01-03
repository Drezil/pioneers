{-# LANGUAGE BangPatterns #-}
module Main where

import           Graphics.UI.Gtk            (AttrOp ((:=)))
import qualified Graphics.UI.Gtk            as Gtk
import qualified Graphics.UI.Gtk.OpenGL     as GtkGL

import qualified Data.Array.IArray          as A
import           Graphics.Rendering.OpenGL  as GL
import qualified Graphics.UI.Gtk.Gdk.EventM as Event

import           Map.Coordinates
import           Map.Map

import           Data.IntSet                as IS
import           Data.IORef
import           Data.Maybe                 (fromMaybe)
import           Debug.Trace

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class     (liftIO)
import           Foreign.Ptr                (nullPtr)
import           GHC.Conc.Sync              (unsafeIOToSTM)
import           Prelude                    as P
import           System.IO.Unsafe           (unsafePerformIO)
import Foreign.Marshal.Array (allocaArray)
import Render.Misc (dumpInfo)

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
                       , dpitch      :: GLfloat
                       , showShadowMap :: Bool }
                       deriving (Show)

type RenderObject = (Vector3 GLfloat, Color3 GLfloat, [Vertex3 GLfloat])

(Vertex4 a b c d) .+ (Vertex4 w x y z) = Vertex4 (a+w) (b+x) (c+y) (d+z)
(Vertex4 a b c d) .* e = Vertex4 (a*e) (b*e) (c*e) (d*e)

animationWaitTime = 3   :: Int
canvasWidth = 1024      :: Int
canvasHeight = 768      :: Int
deltaV = 0.10
deltaH = 0.5
deltaP = 0.15
black = Color3 0 0 0 :: Color3 GLfloat
shadowMapSize :: TextureSize2D
shadowMapSize = TextureSize2D 512 512

up :: Vector3 GLdouble
up = Vector3 0 1 0

origin :: Vertex3 GLdouble
origin   = Vertex3 0 0 0

sun = Light 0 

-- TODO: Put render-stuff in render-modul

--gets Sun position in given format
getSunPos :: (GLdouble -> GLdouble -> GLdouble -> a) -> IO a
getSunPos f = do
        Vertex4 x y z _ <- get (position sun)
        return $ f (realToFrac x) (realToFrac y) (realToFrac z)

glTexCoord2f (x,y) = texCoord (TexCoord2 x y :: TexCoord2 GLfloat)
glVertex3f (x,y,z) = vertex (Vertex3 x y z :: Vertex3 GLfloat)
glNormal3f (x,y,z) = normal (Normal3 x y z :: Normal3 GLfloat)

prepareRenderTile :: PlayMap -> ((Int,Int),MapEntry) -> (Vector3 GLfloat, Color3 GLfloat, [Vertex3 GLfloat])
prepareRenderTile m (c@(cx,cz),(_,t)) =
                        (
                        Vector3 (1.5 * fromIntegral cx) 0.0
                                  (if even cx then 2 * fromIntegral cz else
                                     2 * fromIntegral cz - 1)
                        ,
                        case t of
                                Water -> Color3 0.5 0.5 1 :: Color3 GLfloat
                                Grass -> Color3 0.3 0.9 0.1 :: Color3 GLfloat
                                Sand -> Color3 0.9 0.85 0.7 :: Color3 GLfloat
                                Mountain -> Color3 0.5 0.5 0.5 :: Color3 GLfloat
                        ,getTileVertices m c)

renderTile :: RenderObject -> IO ()
renderTile (coord,c,ts) =
        preservingMatrix $ do
                translate coord
                {-color black
                lineWidth $= 4.0
                lineSmooth $= Enabled
                _ <- renderPrimitive LineLoop $ do
                        glNormal3f(0.0,0.0,1.0)
                        mapM vertex ts-}
                color c
                _ <- renderPrimitive Polygon $ do
                        glNormal3f(0.0,1.0,0.0)
                        mapM vertex ts
                return ()

drawSphere :: IO ()
drawSphere = renderQuadric
  (QuadricStyle (Just Smooth) GenerateTextureCoordinates Outside
     FillStyle)
  (Sphere 2.0 48 48)

drawObjects :: [RenderObject] -> [RenderObject] -> Bool -> IO ()
drawObjects map ent shadowRender = do
    textureOn <- get (texture Texture2D) --are textures enabled?

    when shadowRender $
        texture Texture2D $= Disabled --disable textures if we render shadows.

    --draw something throwing shadows
    preservingMatrix $ do
        pos <- getSunPos Vector3
        translate $ fmap (+ (-15.0)) pos
        drawSphere
    preservingMatrix $ do
        pos <- getSunPos Vector3
        translate $ fmap (+ (-10.0)) pos
        drawSphere
    --draw sun-indicator
    {- preservingMatrix $ do
        pos <- getSunPos Vector3
        translate pos
        color (Color4 1.0 1.0 0.0 1.0 :: Color4 GLfloat)
        drawSphere
        --putStrLn $ unwords ["sun at", show pos]
        -- -}
    --draw map
    mapM_ renderTile map


    when (shadowRender && textureOn == Enabled) $ --reset texture-rendering
        texture Texture2D $= Enabled

-- OpenGL polygon-function for drawing stuff.
display :: MVar ProgramState -> PlayMap -> IO ()
display state t =
  let
     -- Todo: have tiles static somewhere .. dont calculate every frame
     tiles = P.map (prepareRenderTile t) (A.assocs t)
  in
      do
        ps@PS {
          px       = px
        , py       = py
        , pz       = pz
        , pitch    = pitch
        , heading  = heading
        , showShadowMap = showShadowMap }
                <- readMVar state
        loadIdentity
        GL.rotate pitch (Vector3 1.0 0.0 0.0 :: Vector3 GLfloat)
        GL.rotate heading (Vector3 0.0 1.0 0.0 :: Vector3 GLfloat)
        translate (Vector3 (-px) (-py) (-pz)::Vector3 GLfloat)

        generateShadowMap tiles []
        generateTextureMatrix
        unless showShadowMap $ do
                clear [ ColorBuffer, DepthBuffer ]
                preservingMatrix $ do
                        drawObjects tiles [] False

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
        , dheading = dheading
        }
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

-- Note: preservingViewport is not exception safe, but it doesn't matter here
preservingViewport :: IO a -> IO a
preservingViewport act = do
   v <- get viewport
   x <- act
   viewport $= v
   return x

generateTextureMatrix :: IO ()
generateTextureMatrix = do
   -- Set up projective texture matrix. We use the Modelview matrix stack and
   -- OpenGL matrix commands to make the matrix.
   m <- preservingMatrix $ do
      loadIdentity
      -- resolve overloading, not needed in "real" programs
      let translatef = translate :: Vector3 GLfloat -> IO ()
          scalef = scale :: GLfloat -> GLfloat -> GLfloat -> IO ()
      translatef (Vector3 0.5 0.5 0.0)
      scalef 0.5 0.5 1.0
      ortho (-20) 20 (-20) 20 1 100
      lightPos' <- getSunPos Vertex3
      lookAt lightPos' origin up
      get (matrix (Just (Modelview 0)))

   [ sx, sy, sz, sw,
     tx, ty, tz, tw,
     rx, ry, rz, rw,
     qx, qy, qz, qw ] <- getMatrixComponents RowMajor (m :: GLmatrix GLdouble)

   textureGenMode S $= Just (ObjectLinear (Plane sx sy sz sw))
   textureGenMode T $= Just (ObjectLinear (Plane tx ty tz tw))
   textureGenMode R $= Just (ObjectLinear (Plane rx ry rz rw))
   textureGenMode Q $= Just (ObjectLinear (Plane qx qy qz qw))

generateShadowMap :: [RenderObject] -> [RenderObject] -> IO ()
generateShadowMap tiles obj = do
   lightPos' <- getSunPos Vertex3
   let (TextureSize2D shadowMapWidth shadowMapHeight) = shadowMapSize
       shadowMapSize' = Size shadowMapWidth shadowMapHeight

   preservingViewport $ do
      viewport $= (Position 0 0, shadowMapSize')

      clear [ ColorBuffer, DepthBuffer ]
      
      cullFace $= Just Front -- only backsides cast shadows -> less polys

      matrixMode $= Projection
      preservingMatrix $ do
         loadIdentity
         ortho (-20) 20 (-20) 20 10 100
         matrixMode $= Modelview 0
         preservingMatrix $ do
            loadIdentity
            lookAt lightPos' origin up
            drawObjects tiles obj True
         matrixMode $= Projection
      matrixMode $= Modelview 0

      copyTexImage2D Texture2D 0 DepthComponent' (Position 0 0) shadowMapSize 0
      
      cullFace $= Just Back

   when True $ do
      let numShadowMapPixels = fromIntegral (shadowMapWidth * shadowMapHeight)
      allocaArray numShadowMapPixels $ \depthImage -> do
        let pixelData fmt = PixelData fmt Float depthImage :: PixelData GLfloat
        readPixels (Position 0 0) shadowMapSize' (pixelData DepthComponent)
        (_, Size viewPortWidth _) <- get viewport
        windowPos (Vertex2 (fromIntegral viewPortWidth / 2 :: GLfloat) 0)
        drawPixels shadowMapSize' (pixelData Luminance)

--Adjust size to given dimensions
reconfigure :: Int -> Int -> IO (Int, Int)
reconfigure w h = do
  -- maintain aspect ratio
  let aspectRatio = fromIntegral canvasWidth / fromIntegral canvasHeight
      (w1, h1)    = (fromIntegral w, fromIntegral w / aspectRatio)
      (w2, h2)    = (fromIntegral h * aspectRatio, fromIntegral h)
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
                , dheading    = dheading
                , showShadowMap = showShadowMap }
            <- takeMVar state
          -- Only process the key event if it is not a repeat
          (ps',ret) <- if (fromIntegral code `member` kp && not press) ||
             (fromIntegral code `notMember` kp && press)
             then let
                      accept a = return (a, True)
                      deny   a = return (a, False)
                in do
                -- keep list of pressed keys up2date
                ps <- return (if not press then
                                (ps{keysPressed = fromIntegral code `delete` kp})
                              else
                                (ps{keysPressed = fromIntegral code `insert` kp}))
                putStrLn $ unwords [name , show val, show code, show ps] -- trace (unwords [name , show val]) -- debugging
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
                    | code == 42      -> accept $ ps { showShadowMap = not showShadowMap }
                    | code == 31      -> dumpInfo >> accept ps
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
  state <- newMVar PS {        keysPressed = IS.empty
                             , px          = 7.5
                             , py          = 20
                             , pz          = 15
                             , heading     = 0
                             , pitch       = 60
                             , dx          = 0
                             , dy          = 0
                             , dz          = 0
                             , dheading    = 0
                             , dpitch      = 0
                             , showShadowMap = False }
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
    --set up shadow-map
    texImage2D Texture2D NoProxy 0 DepthComponent' shadowMapSize 0
              (PixelData DepthComponent UnsignedByte nullPtr)

    materialAmbient   Front $= Color4 0.4 0.4 0.4 1.0
    materialDiffuse   Front $= Color4 0.4 0.4 0.4 1.0
    materialSpecular  Front $= Color4 0.8 0.8 0.8 1.0
    materialShininess Front $= 25.0

    ambient  sun $= Color4 0.3 0.3 0.3 1.0
    diffuse  sun $= Color4 1.0 1.0 1.0 1.0
    specular sun $= Color4 0.8 0.8 0.8 1.0
    lightModelAmbient  $= Color4 0.2 0.2 0.2 1.0
    position sun $= (Vertex4 2.0 1.0 1.3 1.0 :: Vertex4 GLfloat) .* (1/2.5865) .* 45
    spotDirection sun $= (Normal3 (2.0) (1.0) (1.3) :: Normal3 GLfloat)
    --spotExponent sun $= 1.0
    --attenuation sun $= (1.0, 0.0, 0.0)

    lighting        $= Enabled
    light sun       $= Enabled
    depthFunc       $= Just Less
    shadeModel      $= Smooth
    --lightModelLocalViewer $= Enabled
    --vertexProgramTwoSide $= Enabled

    clearColor $= Color4 0.0 0.0 0.0 0.0
    drawBuffer $= BackBuffers
    colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)

    frontFace $= CCW
    cullFace $= Just Back

    texture Texture2D $= Enabled
    
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    textureCompareMode Texture2D $= Just Lequal
    depthTextureMode Texture2D $= Luminance'

    shadeModel $= Smooth

    fog $= Enabled
    fogMode $= Linear 45.0 50.0
    fogColor $= Color4 0.5 0.5 0.5 1.0
    fogDistanceMode $= EyeRadial


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

