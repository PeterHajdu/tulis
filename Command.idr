module Command

%default total

public export
data Command : Type where
  TurnLeft : Command
  TurnRight : Command
  Forward : Command
