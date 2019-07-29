module Ui

import World
import Terminal
import Data.Vect

export
setup : IO ()
setup = do
  setRaw
  hideCursor
  clearScreen

export
tearDown : IO ()
tearDown = do
  showCursor

showTile : Nat -> Nat -> Tile -> IO ()
showTile x y tile = do
  moveCursor (S x) (S y)
  case tile of
    Empty => putStr "."
    Finish => putStr "x"

showColumn : Vect ylimit Tile -> Nat -> Nat -> IO ()
showColumn Nil _ _ = pure ()
showColumn (tile :: rest) x y = do
  showTile x y tile
  showColumn rest x (S y)

showColumns : Vect cols (Vect rows Tile) -> Nat -> IO ()
showColumns Vect.Nil _ = pure ()
showColumns (col::rest) x = do
  showColumn col x Z
  showColumns rest (S x)

showTurtle : Turtle xlimit ylimit -> IO ()
showTurtle (MkTurtle x y heading) =
  let turtleChar = case heading of
                     North => '^'
                     South => 'v'
                     West => '<'
                     East => '>'
   in do
    moveCursor (finToNat x) (finToNat y)
    putChar turtleChar

export
showWorld : World.World -> IO ()
showWorld (MkWorld turtle grid) = do
  showColumns grid Z
  showTurtle turtle
