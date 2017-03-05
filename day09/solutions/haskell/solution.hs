import Data.List.Split

data Token = Token (Int, Int) deriving (Show)

testData = "1(10x5)23\n(3x3)abc\n"

filteredData = filter (/='\n') testData

decode [] = []

decode ( '(' : xs ) =
  (concat $ replicate times toRepeat) ++ decode toProcess
  where
    tokenStr = takeWhile (/=')') xs
    rest = tail $ dropWhile (/=')') xs
    [count, times] = map read $ splitOn "x" tokenStr
    toRepeat = take count rest
    toProcess = drop count rest
decode ( x : xs ) = x : decode xs

isDone xs = all (/='(') xs

applyUntil cond func xs =
  if (cond xs) then xs else applyUntil cond func $ func xs

main = do
  input <- readFile "data.input"
  let dat = filter (/='\n') input
  let output = applyUntil isDone decode dat
  return (length output)
