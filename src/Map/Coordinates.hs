{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, BangPatterns #-}
module Map.Coordinates 

--exports..
(
getTileVertices,
Tile
)

where

import Graphics.Rendering.OpenGL as GL
import Prelude as P
import Data.Array.IArray as A
import Map.Map as PMap hiding (heightLookup)


type Coordinates = (Int, Int)
type Pos = (Float, Float)

type Tile = Coordinates

instance Num Tile where
        (i,j) + (x,y) = (i+x, j+y)
        (i,j) * (x,y) = (i*x, j*y)
        (i,j) - (x,y) = (i-x, j-y)
        negate (x,y) = (negate x, negate y)
        abs (x,y) = (abs x, abs y)
        signum (_,_) = undefined
        fromInteger a = (fromIntegral a,fromIntegral a)

instance Num Pos where
        (i,j) + (x,y) = (i+x, j+y)
        (i,j) * (x,y) = (i*x, j*y)
        (i,j) - (x,y) = (i-x, j-y)
        negate (x,y) = (negate x, negate y)
        abs (x,y) = (abs x, abs y)
        signum (_,_) = undefined
        fromInteger a = (fromIntegral a, fromIntegral a)

data Neighbours =
          North
        | South
        | NorthEast
        | SouthEast
        | NorthWest
        | SouthWest
        deriving (Show, Eq)

-- | Ordered Vertice-List for rendering (Counterclockwise)
data TileVertex =
          VertexNW
        | VertexNE
        | VertexE
        | VertexSE
        | VertexSW
        | VertexW
        deriving (Show, Eq, Ord)


--Culling is done with GL_CCW
getTileVertices :: PlayMap -> Tile -> [Vertex3 GLfloat]
getTileVertices heights t = let p = (listArray (0,5) hexagon)
                                        ::Array Int (Float,Float) in
                                       P.map floatToVertex $
                                  [
                                        (fst $ p ! 5, getHeight heights VertexW  t, snd $ p ! 5),
                                        (fst $ p ! 4, getHeight heights VertexSW t, snd $ p ! 4),
                                        (fst $ p ! 3, getHeight heights VertexSE t, snd $ p ! 3),
                                        (fst $ p ! 2, getHeight heights VertexE  t, snd $ p ! 2),
                                        (fst $ p ! 1, getHeight heights VertexNE t, snd $ p ! 1),
                                        (fst $ p ! 0, getHeight heights VertexNW t, snd $ p ! 0)
                                  ]

getHeight :: PlayMap -> TileVertex -> Tile -> Float
getHeight pm v t@(tx,_) =
        let 
                h = heightLookup pm
                ! tileheight = h t
                ! y = if even tx then 0 else -1
        in
        case v of
                VertexNW -> let
                                n = h (t+(0,-1))
                                nw = h (t+(-1,y)) 
                            in (n + nw + tileheight) / 3.0
                VertexNE -> let
                                n = h (t+(0,-1))
                                ne = h (t+(1,y))
                            in  (n + ne + tileheight) / 3.0
                VertexE -> let
                                ne = h (t+(1,y))
                                se = h (t+(1,y+1))
                           in  (ne + se + tileheight) / 3.0
                VertexSE -> let
                                s = h (t+(0,1))
                                se = h (t+(1,y+1))
                            in  (s + se + tileheight) / 3.0
                VertexSW -> let
                                s = h (t+(0,1))
                                sw = h (t+(-1,y+1))
                            in  (s + sw + tileheight) / 3.0
                VertexW -> let
                                sw = h (t+(-1,y+1))
                                nw = h(t+(-1,y))
                           in  (sw + nw + tileheight) / 3.0

heightLookup :: PlayMap -> Tile -> Float
heightLookup hs t = if inRange (bounds hs) t then h else 0
                where 
                        (h,_) = hs ! t

hexagon :: [(Float,Float)]
hexagon = [
                (-0.5,-1),
                (0.5,-1),
                (1,0),
                (0.5,1),
                (-0.5,1),
                (-1,0)
                ]


-- | convert triple of floats to GLfloat (== CFloat)
floatToVertex :: (Float, Float, Float) -> Vertex3 GLfloat
floatToVertex (a,b,c) = Vertex3 (realToFrac a::GLfloat) (realToFrac b::GLfloat) (realToFrac c::GLfloat)
