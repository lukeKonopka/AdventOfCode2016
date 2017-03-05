import Data.List.Split
import qualified Data.List as List
import Data.Maybe
import qualified Data.Set as Set

data Position = Position {x :: Int, y :: Int} deriving (Show, Eq, Ord)
data Direction = DUp | DRight | DDown | DLeft deriving (Show, Enum, Eq)
data Turn = TurnLeft | TurnRight | NoTurn deriving (Show, Eq)
data State = State {pos :: Position, dir :: Direction} deriving (Show, Eq)
data Command = Command {len :: Int, turn :: Turn} deriving (Show, Eq)

dup xs =
  dup' xs Set.empty
  where
    dup' [] _ = Nothing
    dup' (x:xs) s =
      if Set.member x s
      then Just x
      else dup' xs (Set.insert x s)

stringToCommand string =
  Command {len=l, turn=t}
  where
    l = read (tail string)
    t = case head string of
      'L' -> TurnLeft
      'R' -> TurnRight

changeDir state command =
  State {pos = pos state, dir = newDir}
  where
    newDir = case turn command of
      TurnRight -> if (dir state) == DLeft then DUp else succ $ dir state
      TurnLeft -> if (dir state) == DUp then DLeft else pred $ dir state
      NoTurn -> dir state

move state command =
  State {pos = newPos, dir = dir state}
  where
    newPos = case dir state of
      DLeft -> Position {x=oldX-1, y=oldY}
      DRight -> Position {x=oldX+1, y=oldY}
      DUp -> Position {x=oldX, y=oldY-1}
      DDown -> Position {x=oldX, y=oldY+1}
    oldX = x$pos$state
    oldY = y$pos$state
    l = len command

applyCommand state command =
  move (changeDir state command) command

commandString = "R2, L1, R2, R1, R1, L3, R3, L5, L5, L2, L1, R4, R1, R3, L5, L5, R3, L4, L4, R5, R4, R3, L1, L2, R5, R4, L2, R1, R4, R4, L2, L1, L1, R190, R3, L4, R52, R5, R3, L5, R3, R2, R1, L5, L5, L4, R2, L3, R3, L1, L3, R5, L3, L4, R3, R77, R3, L2, R189, R4, R2, L2, R2, L1, R5, R4, R4, R2, L2, L2, L5, L1, R1, R2, L3, L4, L5, R1, L1, L2, L2, R2, L3, R3, L4, L1, L5, L4, L4, R3, R5, L2, R4, R5, R3, L2, L2, L4, L2, R2, L5, L4, R3, R1, L2, R2, R4, L1, L4, L4, L2, R2, L4, L1, L1, R4, L1, L3, L2, L2, L5, R5, R2, R5, L1, L5, R2, R4, R4, L2, R5, L5, R5, R5, L4, R2, R1, R1, R3, L3, L3, L4, L3, L2, L2, L2, R2, L1, L3, R2, R5, R5, L4, R3, L3, L4, R2, L5, R5"

-- commandString = "R8, R4, R4, R8"

commandStringList = splitOn ", " commandString

commandList = map stringToCommand commandStringList

commandListUnit = concat $ map (\command ->
  Command{len = 1, turn = turn command} : replicate ((len command)-1) Command{len = 1, turn = NoTurn}) commandList

initialState = State {pos = Position {x = 0,y = 0}, dir = DUp}

-- finalState = foldr (\cmd -> move cmd . changeDir cmd) initialState commandListUnit


stateList = scanl applyCommand initialState commandListUnit

posList = map pos stateList

firstDuplicate = fromJust $ dup posList

distance pos = (abs fX) + (abs fY)
  where
    fX = x$pos
    fY = y$pos
