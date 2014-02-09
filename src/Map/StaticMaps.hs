module Map.StaticMaps
where

import Map.Types

import Data.Array

emptyMap :: PlayMap
emptyMap = array ((0,0), (100,100)) [((a,b), (Minimal (a,b) 0.5)) | a <- [0..100], b <- [0..100]]
