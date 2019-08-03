module Main

import Command
import System
import State
import Data.Fin
import World
import Data.Vect
import Terminal
import Ui
import Input

%default total

data Result : Type where
  Won : Result
  Failed : Result

Eq Result where
  Won == Won = True
  Failed == Failed = True
  _ == _ = False

sleepOneSec : IO ()
sleepOneSec = usleep 1000000

resultOf : World.World -> Result
resultOf world = if (didWin world) then Won else Failed

partial
run : State -> IO Result
run (MkState world Nil) = pure $ resultOf world
run (MkState world (command::rest)) =
  let newWorld = updateWorld command world
      newState = (MkState newWorld rest)
   in do
      showState newState
      sleepOneSec
      if isOver newState
      then pure $ resultOf newWorld
      else run newState

partial
loop : State -> IO Result
loop state@(MkState world program) = do
  showState state
  Just c <- getInput | loop state
  case c of
    Quit => pure Failed
    RunProgram => run state
    AppendCommand command => loop $ MkState world (program ++ [command])
    DeleteCommand => let newProgram = maybe program id (init' program)
                      in loop $ MkState world newProgram

level1 : State
level1 =
  let turtle = MkTurtle FZ FZ North
      grid = [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Finish]]
      world = MkWorld turtle grid
   in (MkState world Nil)

level2 : State
level2 =
  let turtle = MkTurtle FZ FZ North
      grid = [[Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Finish, Empty], [Empty, Empty, Empty, Empty, Finish]]
      world = MkWorld turtle grid
   in (MkState world Nil)

level3 : State
level3 =
  let turtle = MkTurtle FZ FZ North
      grid = [[Empty, Wall, Empty, Empty, Empty], [Empty, Wall, Empty, Finish, Empty], [Empty, Empty, Empty, Wall, Finish]]
      world = MkWorld turtle grid
   in (MkState world Nil)

levels : List State
levels = [level1, level2, level3]

partial
whileM : Monad m => (a -> Bool) -> m a -> m a
whileM pred action = do
  result <- action
  if (pred result)
  then whileM pred action
  else pure result

partial
untilSuccess : State -> IO Result
untilSuccess state = whileM ((==) Failed) (loop state)

partial
runLevels : List State -> IO (List Result)
runLevels = traverse untilSuccess

partial
main : IO ()
main = do
  setup
  runLevels levels
  tearDown
