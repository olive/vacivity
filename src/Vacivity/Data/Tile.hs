module Vacivity.Data.Tile where


data TileType = Wall | Free | Solid deriving (Eq)

data Item

data DTile = DTile [Item] TileType


isFree :: DTile -> Bool
isFree (DTile _ Free) = True
isFree _ = False
