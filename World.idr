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
Show (Turtle x y) where
  show (MkTurtle x y h) = "Turtle " ++ show (finToNat x) ++ " " ++ show (finToNat y) ++ " " ++ show h

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

public export
data Tile : Type where
  Empty : Tile
  Wall : Tile
  Finish : Tile

export
Eq Tile where
  Empty == Empty = True
  Finish == Finish = True
  Wall == Wall = True
  _ == _ = False

public export
data World : Type where
  MkWorld : Turtle xlimit ylimit -> Vect xlimit (Vect ylimit Tile) -> World.World


forwardCoords : Heading -> (Fin xlimit, Fin ylimit) -> (Fin xlimit, Fin ylimit)
forwardCoords North (x, y) = (x, (dec y))
forwardCoords West (x, y) = ((dec x), y)
forwardCoords South (x, y) = (x, (incInBound y))
forwardCoords East (x, y) = ((incInBound x), y)

tileAt : Fin xlimit -> Fin ylimit -> Vect xlimit (Vect ylimit Tile) -> Tile
tileAt x y grid = index y (index x grid)

forward : World.World -> World.World
forward (MkWorld (MkTurtle x y heading) grid) =
  let (maybeNewX, maybeNewY) = forwardCoords heading (x, y)
      tileAtNewPosition = tileAt maybeNewX maybeNewY grid
      (newX, newY) = if tileAtNewPosition == Wall
                     then (x, y)
                     else (maybeNewX, maybeNewY)
   in MkWorld (MkTurtle newX newY heading) grid

export
updateWorld : Command -> World.World -> World.World
updateWorld TurnLeft (MkWorld turtle grid) = MkWorld (turnLeft turtle) grid
updateWorld TurnRight (MkWorld turtle grid) = MkWorld (turnRight turtle) grid
updateWorld Forward world = forward world

export
didWin : World.World -> Bool
didWin (MkWorld (MkTurtle x y _) grid) = tileAt x y grid == Finish
