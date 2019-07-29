module Main

import Data.Fin
import World
import Data.Vect
import Terminal
import Ui

%default total

turtle : Turtle 3 3
turtle = MkTurtle FZ FZ North

grid : Vect 3 (Vect 3 Tile)
grid = [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Finish]]

world : World.World
world = MkWorld turtle grid


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
