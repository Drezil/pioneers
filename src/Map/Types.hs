module Map.Types
where

import PioneerTypes

newtype XCoord  = XCoord Int
newtype YCoord  = YCoord Int
newtype ZCoord  = ZCoord Float

newtype  Coord  = Coord (XCoord, YCoord, ZCoord) 

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

data Node = Full    Coord TileType BuildInfo PlayerInfo PathInfo ResInfo StorInfo
          | Minimal Coord
