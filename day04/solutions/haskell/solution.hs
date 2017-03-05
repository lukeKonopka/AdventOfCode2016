import qualified Data.List.Split as List.Split
import Data.List

data Room = Room {name :: String, orgName :: String, iden :: Int, checkSum :: String} deriving (Show)

stringToRoom string =
  Room {name = n, orgName = on, iden = i, checkSum = c}
  where
    c = init $ lastN 6 string
    i = read $ last nameWithId
    n = concat $ init nameWithId
    on = (intercalate " ") . init $ nameWithId
    nameWithId = List.Split.splitOn "-" $ take ((length string)-7) string
    lastN n xs = drop (length xs - n) xs

checkIfCorrect room =
  (makeCheckSum (name room)) == (checkSum room)
  where
    makeCheckSum str = take 5 . concat . map (fst) . sortBy (sortFunc) . group' $ str
    group' s = map (\x->([head x], length x)) . group . sort $ s
    sortFunc (v1, count1) (v2, count2) =
      if count1 == count2
      then compare v1 v2
      else compare count2 count1

decipherName name times =
  map (\char -> iterate turnChar char !! times) name
  where
    turnChar ' ' = ' '
    turnChar 'z' = 'a'
    turnChar c = succ c

calculateResult inputData =
  decipheredNames
  where
    l = List.Split.splitOn "\n" inputData
    lastCut = init l
    rooms = (map stringToRoom) lastCut
    correctRooms = filter checkIfCorrect rooms
    sumOfIds = sum $ map (iden) correctRooms
    decipheredNames = map (\room -> ((iden $ room), (`decipherName` (iden $ room)) . orgName $ room)) correctRooms

main :: IO ()
main = do
  input <- readFile "dataset.input"
  let result = calculateResult input
  let resAnnotated = map (\res -> ((show . fst) res) ++ ("\t\t" ++ (snd res))) result
  let resultString = intercalate "\n" resAnnotated
  writeFile "result.out" resultString
