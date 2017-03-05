import Data.List.Split as Split
import Data.List as List

data Ip = Ip {free :: [String], hypernet :: [String]} deriving Show

isAba :: String -> Bool
isAba [a, b, c] = a==c && a/=b

abaToBab :: String -> String
abaToBab [a, b, c] = [b,a,b]

getAbas :: String -> [String]
getAbas str = filter isAba (takeAll 3 str)

findAba :: String -> Bool
findAba str = length (getAbas str) /= 0

findBab :: String -> String -> Bool
findBab str aba =
  any (==bab) $ getAbas str
  where
    bab = abaToBab aba

takeAll :: Int -> String -> [String]
takeAll len str
  | length str < len = []
  | otherwise = (take len str) : takeAll len (tail str)

stringToIp :: String -> Ip
stringToIp str =
  Ip {free = f, hypernet = hn}
  where
    (f, hn) = split sections
    split = foldr (\x ~(y2,y1) -> (x:y1, y2)) ([],[])
    sections = Split.splitOneOf "[]" str

isQualified :: Ip -> Bool
isQualified ip =
  any (`elem` babsInverted) abas
  where
    abas = concat $ map getAbas (free ip)
    babs = concat $ map getAbas (hypernet ip)
    babsInverted = map abaToBab babs

main :: IO Int
main = do
  input <- readFile "data.input"
  let lines = List.lines input
  let ips = map stringToIp lines
  let qualified = filter isQualified ips
  let result = length qualified
  return result
