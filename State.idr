module State

import World
import Command

public export
data State : Type where
  MkState : World.World -> List Command -> State

export
isOver : State -> Bool
isOver (MkState world Nil) = True
isOver (MkState world _) = didWin world
