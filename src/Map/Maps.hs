module Map.Maps
where

import Map.Types

-- potentially to be expanded to Nodes
giveNeighbours :: (Int, Int) -> [(Int,Int)]
giveNeighbours (x,y) = filter (not . negative) all
  where
    all = if even y then [(x+1,y), (x-1,y), (x,y+1), (x,y-1), (x+1,y+1), (x+1,y-1)]
                    else [(x+1,y), (x-1,y), (x,y+1), (x,y-1), (x-1,y+1), (x-1,y-1)]

    negative :: (Int, Int) -> Bool
    negative (x,y) = x < 0 || y < 0
