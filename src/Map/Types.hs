module Map.Types
where

import PioneerTypes

import Data.Array

type PlayMap = Array (XCoord, YCoord) Node 

type XCoord  = Int
type YCoord  = Int
type ZCoord  = Float 

-- | Ownership information, Parameter to occupied is player number
data PlayerInfo = NoPlayer
                | Occupied Int

-- | Path info, is this node part of a path?
data PathInfo   = NoPath
                | Path
                | Border

-- | What resources can be harvested here?
data ResInfo    = ResInfo  Resource  Amount

-- | What commodities are currently stored here?
data StorInfo   = StorInfo Commodity Amount

-- | What kind of structures may be erected here?
data BuildInfo  = BStruc Structure 
                | BFlag
                | BSmall
                | BMedium
                | BLarge

data TileType   = Ocean
                | Beach
                | Grass
                | Desert
                | Lake
                | Hill     -- ^ Accessible
                | Mountain -- ^ Not accessible
                deriving (Eq)

-- TODO: Record Syntax
data Node = Full    (XCoord, YCoord) ZCoord TileType BuildInfo PlayerInfo PathInfo ResInfo StorInfo
          | Minimal (XCoord, YCoord) ZCoord -- defaults to empty green grass node on height 0.5

