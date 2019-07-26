module Main

import Turtle
import Data.Fin
import World
import Data.Vect
import Terminal

%default total

turtle : Turtle 3 3
turtle = MkTurtle FZ FZ North

grid : Vect 3 (Vect 3 Tile)
grid = [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Finish]]

world : World.World
world = MkWorld turtle grid

setup : IO ()
setup = do
  setRaw
  hideCursor
  clearScreen

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

showWorld : World.World -> IO ()
showWorld (MkWorld turtle grid) = do
  showColumns grid Z
  showTurtle turtle

partial
loop : World.World -> IO ()
loop world = do
  showWorld world
  getChar
  loop world

partial
main : IO ()
main = do
  setup
  loop world
  tearDown
