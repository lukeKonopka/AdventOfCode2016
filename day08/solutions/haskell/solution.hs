
import Data.List
import qualified Data.List.Split as Split

data Screen = Screen {data' :: [[Bool]]}

instance Show Screen where
  show scr = screenToString scr where
    screenToString screen =
      unlines $ map (\line -> (map stateToChar line)) (data' screen)
      where
        stateToChar True = '#'
        stateToChar False = ' '

data Command =
  Rect {width :: Int, height :: Int} |
  Rotate {number :: Int, howFar :: Int, dir :: Direction} deriving (Show)

data Direction = Columns | Rows deriving (Eq, Show)

initScreen w h = Screen{data'=replicate h (replicate w False)}

printScreen screen = do
  let string = screenToString screen
  putStr string
  where
    screenToString screen =
      unlines $ map (\line -> (map stateToChar line)) (data' screen)
      where
        stateToChar True = '#'
        stateToChar False = ' '

fill :: Int -> Int -> [[Bool]] -> [[Bool]]
fill x y matrix =
  addList (\l1 l2 -> addList (||) l1 l2) filledMatrix matrix
  where
    filledMatrix = (replicate y) . (replicate x) $ True

rect :: Screen -> Command -> Screen
rect screen cmd =
  Screen {data'=dat}
  where
    dat = fill (width cmd) (height cmd) (data' screen)

rotate :: Int -> [t] -> [t]
rotate n list
  | n == 0 = list
  | otherwise = rotate (n-1) ((last list) : (init list))

replace :: Int -> t -> [t] -> [t]
replace n el xs = (take n xs) ++ [el] ++ (drop (n + 1) xs)

rotateColumn :: Screen -> Command -> Screen
rotateColumn screen cmd =
  Screen {data' = dat}
  where
    dat = transpose res
    res = replace index rotated transposed
    rotated = (rotate n) . (!! index) $ transposed
    transposed = transpose (data' screen)
    index = number cmd
    n = howFar cmd

rotateRow :: Screen -> Command -> Screen
rotateRow screen cmd =
  Screen {data' = dat}
  where
    dat  = replace index rotated (data' screen)
    rotated = (rotate n) . (!! index) $ (data' screen)
    index = number cmd
    n = howFar cmd

rotateBase :: Screen -> Command -> Screen
rotateBase screen command
  | (dir command) == Columns = rotateColumn screen command
  | (dir command) == Rows = rotateRow screen command

addList :: (t -> t -> t) -> [t] -> [t] -> [t]
addList adder a b
  | (length a) < (length b) = addListHelp a b
  | otherwise = addListHelp b a
  where
    addListHelp a b =
      (map addPair (zip a b)) ++ drop (length a) b
    addPair (x, y) = adder x y

stringToCommand str
  | (take 4 str) == "rect" = stringToRect str
  | (take 6 str) == "rotate" = stringToRotate str
  where
    stringToRect str = Rect {width=(head dims), height=(last dims)}
      where
        dimensionStr = drop 5 str
        dimensions = Split.splitOn "x" dimensionStr
        dims = map (\d -> read d::Int) dimensions
    stringToRotate str = Rotate {number=n, howFar=hf, dir=d}
      where
        d = if (head $ drop 7 str) == 'c' then Columns else Rows
        numbers = Split.splitOn " by " ((Split.splitOn "=" str) !! 1)
        n = read $ head numbers
        hf = read $ last numbers

calculate commandList =
  lastScreen
  where
    lastScreen = foldr folder initialScreen commandList
    initialScreen = initScreen 50 6
    folder cmd screen = case cmd of
      Rect{width=_, height=_} -> rect screen cmd
      Rotate{number=_, howFar=_, dir=_} -> rotateBase screen cmd

folder cmd screen = case cmd of
  Rect{width=_, height=_} -> rect screen cmd
  Rotate{number=_, howFar=_, dir=_} -> rotateBase screen cmd


main :: IO Screen
main = do
  input <- readFile "data.input"
  let l = lines input
  let commands = map stringToCommand l
  let lastScreen = calculate commands
  printScreen lastScreen
  return lastScreen
