module Map.Types
where

import Types

import Data.Array

type PlayMap = Array (XCoord, ZCoord) Node 

type XCoord  = Int
type ZCoord  = Int
type YCoord  = Float

data MapType    = GrassIslandMap
                | DesertMap

-- | Ownership information, Parameter to occupied is player number
data PlayerInfo = NoPlayer
                | Occupied Int

instance Show PlayerInfo where
    show (NoPlayer)   = "not occupied"
    show (Occupied i) = "occupied by player " ++ show i

-- | Path info, is this node part of a path and if so, where does it lead?
data PathInfo   = NoPath
                | Border
                | Paths [(XCoord, YCoord)]
                deriving (Show, Eq)

-- | What resources can be harvested here?
data ResInfo    = Plain
                | ResInfo  Resource  Amount

instance Show ResInfo where
    show (Plain)           = "no resources"
    show (ResInfo res amt) = "Resource: " ++ show res ++ "," ++ show amt

-- | What commodities are currently stored here?
type StorInfo   = [(Commodity,Amount)]

-- | What kind of structures may be erected here?
data BuildInfo  = BStruc Structure
                | BNothing 
                | BFlag
                | BMine
                | BSmall
                | BMedium
                | BLarge

instance Show BuildInfo where
    show (BStruc s) = "Structure: " ++ show s
    show (BNothing) = "no Structure possible"
    show (BFlag)    = "only flags possible"
    show (BMine)    = "mines possible"
    show (BSmall)   = "small buildings possible"
    show (BMedium)  = "medium buildings possible"
    show (BLarge)   = "large buildings possible"

data TileType   = Ocean
                | Beach
                | Grass
                | Desert
                | Lake
                | Hill     -- ^ Accessible
                | Mountain -- ^ Not accessible
                deriving (Show, Eq)

-- TODO: Record Syntax
data Node = Full    (XCoord, ZCoord) YCoord TileType BuildInfo PlayerInfo PathInfo ResInfo StorInfo
          | Minimal (XCoord, ZCoord) -- defaults to empty green grass node on height 1
          deriving (Show)
