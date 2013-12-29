{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, BangPatterns #-}
module Map.Coordinates 

--exports..
(getTileVertices)

where

import Graphics.Rendering.OpenGL as GL
import Data.Map as M hiding ((!))
import qualified Data.Map as M ((!))
import Data.Maybe
import Prelude as P
import Data.Array.IArray as A



type Coordinates = (Integer, Integer)
type Pos = (Float, Float)

-- | a Tile is 1 unit in size. Due to hexagonality the real rendered Area is less.
type Tile = Coordinates
-- | The heights of a Map in a random accessible way. 
type MapHeights = Map Coordinates Int

instance Num Tile where
        (i,j) + (x,y) = (i+x, j+y)
        (i,j) * (x,y) = (i*x, j*y)
        (i,j) - (x,y) = (i-x, j-y)
        negate (x,y) = (negate x, negate y)
        abs (x,y) = (abs x, abs y)
        signum (_,_) = undefined
        fromInteger a = (a,a)

instance Num Pos where
        (i,j) + (x,y) = (i+x, j+y)
        (i,j) * (x,y) = (i*x, j*y)
        (i,j) - (x,y) = (i-x, j-y)
        negate (x,y) = (negate x, negate y)
        abs (x,y) = (abs x, abs y)
        signum (_,_) = undefined
        fromInteger a = (fromIntegral a, fromIntegral a)

tileToPos :: Tile -> Pos
tileToPos (x,y) = (fromIntegral x, fromIntegral y)

data Neighbours =
          North
        | South
        | NorthEast
        | SouthEast
        | NorthWest
        | SouthWest
        deriving (Show, Eq)

-- | Ordered Vertice-List for rendering (clockwise)
data TileVertex =
          VertexNW
        | VertexNE
        | VertexE
        | VertexSE
        | VertexSW
        | VertexW
        deriving (Show, Eq, Ord)

--getGrid :: Coordinates -> Coordinates -> []

getTileVertices :: MapHeights -> Tile -> [Vertex3 GLfloat]
getTileVertices heights t = let p = (listArray (0,5) $ P.map (+ tileToPos t) hexagon)::Array Int (Float,Float) in
                                        P.map floatToVertex $
                                  [
                                        (fst $ p ! 0, snd $ p ! 0,fromMaybe 0.0 $ getHeight heights VertexNW t),
                                        (fst $ p ! 1, snd $ p ! 1,fromMaybe 0.0 $ getHeight heights VertexNW t),
                                        (fst $ p ! 2, snd $ p ! 2,fromMaybe 0.0 $ getHeight heights VertexNW t),
                                        (fst $ p ! 3, snd $ p ! 3,fromMaybe 0.0 $ getHeight heights VertexNW t),
                                        (fst $ p ! 4, snd $ p ! 4,fromMaybe 0.0 $ getHeight heights VertexNW t),
                                        (fst $ p ! 5, snd $ p ! 5,fromMaybe 0.0 $ getHeight heights VertexNW t)
                                  ]

getHeight :: MapHeights -> TileVertex -> Tile -> Maybe Float
getHeight h v t@(_,ty) =
        let 
                ! tileheight = fmap fromIntegral $ M.lookup t h
                ! y = if even ty then -1 else 0
        in
        case v of
                VertexNW -> do
                                c <- tileheight
                                n <- M.lookup (t+(0,-1)) h
                                nw <- M.lookup (t+(-1,y)) h
                                return $ (fromIntegral $ n+nw+c) / 3.0
                VertexNE -> do
                                c <- tileheight
                                n <- M.lookup (t+(0,-1)) h
                                ne <- M.lookup (t+(1,y)) h
                                return $ (fromIntegral $ n+ne+c) / 3.0
                VertexE -> do
                                c <- tileheight
                                ne <- M.lookup (t+(1,y)) h
                                se <- M.lookup (t+(1,y+1)) h
                                return $ (fromIntegral $ ne+se+c) / 3.0
                VertexSE -> do
                                c <- tileheight
                                s <- M.lookup (t+(0,1)) h
                                se <- M.lookup (t+(1,y+1)) h
                                return $ (fromIntegral $ s+se+c) / 3.0
                VertexSW -> do
                                c <- tileheight
                                s <- M.lookup (t+(0,1)) h
                                sw <- M.lookup (t+(-1,y+1)) h
                                return $ (fromIntegral $ s+sw+c) / 3.0
                VertexW -> do
                                c <- tileheight
                                sw <- M.lookup (t+(-1,y+1)) h
                                nw <- M.lookup (t+(-1,y)) h
                                return $ (fromIntegral $ sw+nw+c) / 3.0


hexagon :: [(Float,Float)]
hexagon = [
                (0.2,0),
                (0.6,0),
                (0.5,1),
                (1,0.6),
                (1,0.2),
                (0.5,0)
                ]


-- | convert triple of floats to GLfloat (== CFloat)
floatToVertex :: (Float, Float, Float) -> Vertex3 GLfloat
floatToVertex (a,b,c) = Vertex3 (realToFrac a::GLfloat) (realToFrac b::GLfloat) (realToFrac c::GLfloat)
