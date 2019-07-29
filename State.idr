module State

import World

public export
data Command : Type where
  TurnLeft : Command
  TurnRight : Command
  Forward : Command

public export
data State : Type where
  MkState : World.World -> List Command -> State

