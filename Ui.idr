module Ui

import State
import World
import Terminal
import Data.Vect

export
setup : IO ()
setup = do
  setRaw
  hideCursor

export
tearDown : IO ()
tearDown = do
  showCursor

showTile : Nat -> Nat -> Tile -> IO ()
showTile x y tile = do
  moveCursor (S x) (S y)
  case tile of
    Empty => do
      setColour White
      putStr "."
    Finish => do
      setColour Green
      putStr "x"

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
    setColour Yellow
    moveCursor (S (finToNat x)) (S (finToNat y))
    putChar turtleChar

commandToChar : Command -> Char
commandToChar TurnLeft = '<'
commandToChar TurnRight = '>'
commandToChar Forward = '^'

showProgram : List Command -> Nat -> IO ()
showProgram commands row =
  let commandString = pack $ map commandToChar commands
   in do
      setColour Blue
      moveCursor Z (S row)
      putStrLn commandString

rows : Vect xlimit (Vect ylimit a) -> Nat
rows _ {ylimit} = ylimit

export
showState : State -> IO ()
showState (MkState (MkWorld turtle grid) program) = do
  clearScreen
  showColumns grid Z
  showTurtle turtle
  showProgram program (rows grid)
