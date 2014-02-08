module PioneerTypes 
where

data Structure = Flag    -- Flag
               | Barrack -- Small
               | Sawmill -- Medium
               | Castle  -- Large

data Amount    = Infinite   -- Neverending supply
               | Finite Int -- Finite supply 

-- Extremely preliminary, expand when needed
data Commodity = WoodPlank
               | Sword
               | Fish

data Resource  = Coal
               | Iron
               | Gold
               | Granite
               | Water
               | Fishes
               deriving Eq
