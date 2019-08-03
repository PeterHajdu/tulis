module State

import World
import Command

public export
data State : Type where
  MkState : World.World -> List Command -> State

