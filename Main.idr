module Main

import System
import State
import Data.Fin
import World
import Data.Vect
import Terminal
import Ui

%default total

data Result : Type where
  Won : Result
  Failed : Result

Eq Result where
  Won == Won = True
  Failed == Failed = True
  _ == _ = False

data Input : Type where
  Quit : Input
  RunProgram : Input
  AppendCommand : Command -> Input
  DeleteCommand : Input

getArrow : IO (Maybe Input)
getArrow = do
  c <- getChar
  pure $ case c of
           'A' => Just $ AppendCommand Forward
           'C' => Just $ AppendCommand TurnRight
           'D' => Just $ AppendCommand TurnLeft
           _   => Nothing

getSpecial : IO (Maybe Input)
getSpecial = do
  c <- getChar
  case c of
    '[' => getArrow
    _ => pure Nothing

getInput : IO (Maybe Input)
getInput = do
  c <- getChar
  case (ord c) of
    27 => getSpecial
    113 => pure $ Just Quit
    104 => pure $ Just $ AppendCommand TurnLeft
    108 => pure $ Just $ AppendCommand TurnRight
    106 => pure $ Just $ AppendCommand Forward
    13  => pure $ Just RunProgram
    127 => pure $ Just DeleteCommand
    _   => pure $ Nothing

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

levels : List State
levels = [level1, level2]

partial
whileM : Monad m => (a -> Bool) -> m a -> m a
whileM pred action = do
  result <- action
  if (pred result)
  then whileM pred action
  else pure result

partial
runLevels : List State -> IO ()
runLevels Nil = putStrLn "You solved all puzzles!"
runLevels allLevels@(nextLevel::rest) = do
  whileM ((==) Failed) (loop nextLevel)
  runLevels rest

partial
main : IO ()
main = do
  setup
  runLevels levels
  tearDown
