module World

import Turtle
import Data.Vect

%default total

public export
data Tile : Type where
  Empty : Tile
  Finish : Tile

public export
data World : Type where
  MkWorld : Turtle xlimit ylimit -> Vect xlimit (Vect ylimit Tile) -> World.World

