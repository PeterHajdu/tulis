module World

import Data.Vect
import Data.Fin

%default total

public export
data Heading : Type where
  North : Heading
  South : Heading
  West : Heading
  East : Heading

Show Heading where
  show North = "North"
  show South = "South"
  show West = "West"
  show East = "East"

public export
data Turtle : Nat -> Nat -> Type where
  MkTurtle : Fin xlimit -> Fin ylimit -> Heading -> Turtle xlimit ylimit

export
Show (Turtle x y) where
  show (MkTurtle x y h) = "Turtle " ++ show (finToNat x) ++ " " ++ show (finToNat y) ++ " " ++ show h

public export
data Tile : Type where
  Empty : Tile
  Finish : Tile

public export
data World : Type where
  MkWorld : Turtle xlimit ylimit -> Vect xlimit (Vect ylimit Tile) -> World.World

