module Input

import Command

public export
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

export
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
