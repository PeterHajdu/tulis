module Main

import System
import State
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

data Input : Type where
  Quit : Input
  RunProgram : Input
  AppendCommand : Command -> Input
  DeleteCommand : Input

validateInput : Char -> Maybe Input
validateInput 'q' = Just Quit
validateInput 'h' = Just $ AppendCommand TurnLeft
validateInput 'l' = Just $ AppendCommand TurnRight
validateInput 'j' = Just $ AppendCommand Forward
validateInput 'r' = Just RunProgram
validateInput 'd' = Just DeleteCommand
validateInput _ = Nothing

updateWorld : Command -> World.World -> World.World
updateWorld TurnLeft (MkWorld turtle grid) = MkWorld (turnLeft turtle) grid
updateWorld TurnRight (MkWorld turtle grid) = MkWorld (turnRight turtle) grid
updateWorld Forward (MkWorld turtle grid) = MkWorld (forward turtle) grid

sleepOneSec : IO ()
sleepOneSec = usleep 1000000

didWin : World.World -> Bool
didWin (MkWorld (MkTurtle x y _) grid) =
  let column = index x grid
      tile = index y column
   in tile == Finish

isOver : State -> Bool
isOver (MkState world Nil) = True
isOver (MkState world _) = didWin world

partial
run : State -> IO ()
run (MkState world Nil) = pure ()
run (MkState world (command::rest)) =
  let newWorld = updateWorld command world
      newState = (MkState newWorld rest)
   in do
      showState newState
      sleepOneSec
      if isOver newState
      then putStrLn $ if didWin newWorld
                      then "You Won!"
                      else "Try again!"
      else run newState

partial
loop : State -> IO ()
loop state@(MkState world program) = do
  showState state
  Just c <- validateInput <$> getChar | loop state
  case c of
    Quit => pure ()
    RunProgram => run state
    AppendCommand command => loop $ MkState world (program ++ [command])
    DeleteCommand => let newProgram = maybe program id (init' program)
                      in loop $ MkState world newProgram

partial
main : IO ()
main = do
  setup
  loop (MkState world Nil)
  tearDown
