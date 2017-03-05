import qualified Data.List.Split as LSplit
import qualified Data.List as List

exampleData = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"

solve inputData =
  result
  where
    dataList = LSplit.splitOn "\n" inputData
    transposed = List.transpose dataList
    grouped = map (group') transposed
    sorted = map (\letter -> head $ List.sortOn (snd) letter) grouped
    result = concat $ map (fst) sorted
    group' s = map (\x->([head x], length x)) . List.group . List.sort $ s

main :: IO String
main = do
  inputData <- readFile "dataset.input"
  let lastCut = init inputData
  let result = solve lastCut
  return result
