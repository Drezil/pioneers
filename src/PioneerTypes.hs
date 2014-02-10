module PioneerTypes 
where

data Structure = Flag           -- Flag
               | Woodcutter     -- Huts
               | Forester
               | Stonemason
               | Fisher
               | Hunter
               | Barracks
               | Guardhouse
               | LookoutTower
               | Well
               | Sawmill        -- Houses
               | Slaughterhouse
               | Mill
               | Bakery
               | IronSmelter
               | Metalworks
               | Armory
               | Mint
               | Shipyard
               | Brewery
               | Storehouse
               | Watchtower
               | Catapult
               | GoldMine       -- Mines
               | IronMine
               | GraniteMine
               | CoalMine
               | Farm           -- Castles
               | PigFarm
               | DonkeyBreeder
               | Harbor
               | Fortress
               deriving Eq

data Amount    = Infinite   -- Neverending supply
               | Finite Int -- Finite supply 

-- Extremely preliminary, expand when needed
data Commodity = WoodPlank
               | Sword
               | Fish
               deriving Eq

data Resource  = Coal
               | Iron
               | Gold
               | Granite
               | Water
               | Fishes
               deriving Eq
