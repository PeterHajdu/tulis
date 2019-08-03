module World

import Data.Vect
import Data.Fin
import Command

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

turnLeftHeading : Heading -> Heading
turnLeftHeading North = West
turnLeftHeading South = East
turnLeftHeading West = South
turnLeftHeading East = North

turnRightHeading : Heading -> Heading
turnRightHeading North = East
turnRightHeading South = West
turnRightHeading West = North
turnRightHeading East = South

public export
data Turtle : Nat -> Nat -> Type where
  MkTurtle : Fin xlimit -> Fin ylimit -> Heading -> Turtle xlimit ylimit

export
turnLeft : Turtle x y -> Turtle x y
turnLeft (MkTurtle x y heading) = MkTurtle x y (turnLeftHeading heading)

export
turnRight : Turtle x y -> Turtle x y
turnRight (MkTurtle x y heading) = MkTurtle x y (turnRightHeading heading)

dec : Fin n -> Fin n
dec FZ = FZ
dec (FS x) = weaken x

incInBound : Fin n -> Fin n
incInBound n = case (strengthen (FS n)) of
                 Left _ => n
                 Right new => new

export
forward : Turtle x y -> Turtle x y
forward (MkTurtle x y North) = MkTurtle x (dec y) North
forward (MkTurtle x y West) = MkTurtle (dec x) y West
forward (MkTurtle x y South) = MkTurtle x (incInBound y) South
forward (MkTurtle x y East) = MkTurtle (incInBound x) y East

export
Show (Turtle x y) where
  show (MkTurtle x y h) = "Turtle " ++ show (finToNat x) ++ " " ++ show (finToNat y) ++ " " ++ show h

public export
data Tile : Type where
  Empty : Tile
  Finish : Tile

export
Eq Tile where
  Empty == Empty = True
  Finish == Finish = True
  _ == _ = False

public export
data World : Type where
  MkWorld : Turtle xlimit ylimit -> Vect xlimit (Vect ylimit Tile) -> World.World

export
updateWorld : Command -> World.World -> World.World
updateWorld TurnLeft (MkWorld turtle grid) = MkWorld (turnLeft turtle) grid
updateWorld TurnRight (MkWorld turtle grid) = MkWorld (turnRight turtle) grid
updateWorld Forward (MkWorld turtle grid) = MkWorld (forward turtle) grid

export
didWin : World.World -> Bool
didWin (MkWorld (MkTurtle x y _) grid) =
  let column = index x grid
      tile = index y column
   in tile == Finish
