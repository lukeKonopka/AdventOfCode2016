import Data.List.Split

data Token =
    IntToken Int |
    NToken {nt :: Int, nr :: [Token]} |
    StrToken {t :: Int, r :: String} deriving Show

d = "X(8x2)(3x3)ABCY"

decode :: String -> [Token]
decode [] = []
decode ('(':xs) =
  nToken : decode rest
  where
    nToken = NToken {nt=times, nr=(decode $ r strToken)}
    strToken = StrToken {t=times, r=toRepeat}
    str = takeWhile (/=')') xs
    [howMany, times] = map read $ splitOn "x" str
    toRepeat = take howMany tRest
    (_:tRest) = dropWhile (/=')') xs
    rest = drop howMany tRest
decode xs =
  (IntToken $ length nonTokens) : (decode rest)
  where
    nonTokens = takeWhile (/='(') xs
    rest = dropWhile (/='(') xs

len :: [Token] -> Int
len (IntToken (x) : xs) = x + len xs
len ((NToken {nt=v, nr=rest}) : xs) = v * len rest + len xs
len [] = 0

main = do
  input <- readFile "data.input"
  let dat = filter (/='\n') input
  let output = decode dat
  return $ len output
