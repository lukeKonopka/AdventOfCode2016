import qualified Data.List as List

data IpAdress = IpAdress {start :: String, hypernet :: String, end :: String}

checkIfAbba str =
  a == (reverse b) && (diff a)
  where
    (a, b) = List.splitAt 2 str
    diff x = (head x) /= (head $ tail x)

containsAbba [] = False
containsAbba h : t =
