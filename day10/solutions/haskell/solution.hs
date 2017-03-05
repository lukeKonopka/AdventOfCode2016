import Data.List
import Text.Read

data Destination = ToBot Int | ToOut Int | None deriving (Show)

data Bot = Bot {
  num :: Int,
  lowTo :: Destination,
  highTo :: Destination,
  values :: (Maybe Int, Maybe Int)
}

type Out = (Int, [Int])

data Output = Output{d :: Out}

instance Show Output where
  show Output{d=(num, [only])} = "- Out "++(show num)++" with only "++(show only)++"\n"
  show Output{d= (num, more)} = "# Out "++(show num)++" with "++(show more)++"\n"

instance Show Bot where
  show Bot {values=vs, num=n} = "#"++(show n)++" "++(show vs)

setProgram :: Bot -> (Destination, Destination) -> Bot
setProgram b (low, high) = Bot {
  num=num b,
  lowTo=low,
  highTo=high,
  values=values b
}

give :: Bot -> Int -> Bot
give b@Bot{values=(Just _, Just _), num=n} v =
  error $ "Bot "++(show n)++" already have two chips"
give b@Bot{values=(Nothing, Nothing)} v = b{values=(Just v, Nothing)}
give b@Bot{values=(x, Nothing)} v = b{values=(x, Just v)}
give b@Bot{values=(Nothing, x)} v = b{values=(Just v, x)}

giveToNum bots n v
  | any ((==n) . num) bots = givenBot : rest
  | otherwise = newBot : bots
  where
    Just botToGiveTo = find ((==n) . num) bots
    givenBot = give botToGiveTo v
    rest = filter ((/=n) . num) bots
    newBot = Bot {num=n, lowTo=None, highTo=None, values=(Just v, Nothing)}

giveToOutput (n, values) val = (n, val : values)
giveToOutputs outs n val
  | any ((==n) . fst) outs = (giveToOutput outToGiveTo val) : rest
  | otherwise = newOut : outs
  where
    Just outToGiveTo = find ((==n) . fst) outs
    rest = filter ((/=n) . fst) outs
    newOut = (n, [val])

setProgramIn bots n v@(lo, hi)
  | any ((==n) . num) bots = setBot : rest
  | otherwise = newBot : bots
  where
    Just botToSetIn = find ((==n) . num) bots
    setBot = setProgram botToSetIn v
    rest = filter ((/=n) . num) bots
    newBot = Bot {num=n, lowTo=lo, highTo=hi, values=(Nothing, Nothing)}

parseCommand bots str@('v':_) =
  giveToNum bots n v
  where
    intValues = (map readMaybe $ words str) :: [Maybe Int]
    Just n = intValues !! 5
    Just v = intValues !! 1
parseCommand bots str@('b':_) =
  setProgramIn bots n (dLow, dHigh)
  where
    ws = words str
    intValues = (map readMaybe ws) :: [Maybe Int]
    Just n = intValues !! 1
    (Just lo, Just hi) = (intValues !! 6, intValues !! 11)
    dLow = parseDest (ws !! 5) lo
    dHigh = parseDest (ws !! 10) hi
    parseDest word val =
      if (head word)=='b' then
        ToBot val
      else
        ToOut val

generateBots xs =
  foldl parseCommand [] commands
  where
    commands = lines xs

pairExists (l, h) =
  any (checkIfMatches (l, h))

checkIfMatches (l, h) bot =
  (l, h) == (values bot) || (h, l) == (values bot)

haveBoth Bot{values=(Just _, Just _)} = True
haveBoth _ = False
anyHaveBoth ((bots, outs, _):_) = any haveBoth bots


transfer states@((bots, outs, theSame):_) =
  (clearedBots++newBots, newOuts, nowTheSame || theSame) : states
  where
    toTransfer = filter haveBoth bots
    untouched = filter (not . haveBoth) bots
    actions = concat $ map split toTransfer
    split Bot{lowTo=lt, highTo=ht, values=(Just x, Just y)} = [(lt, min x y), (ht, max x y)]
    dispatch (bots, outputs) (ToOut num, val) = (bots, giveToOutputs outputs num val)
    dispatch (bots, outputs) (ToBot num, val) = (giveToNum bots num val, outputs)
    (newBots, newOuts) = foldl dispatch initialState actions
    initialState = (untouched, outs)
    clear bot = bot{values=(Nothing, Nothing)}
    clearAll bots = map clear bots
    clearedBots = clearAll toTransfer
    nowTheSame = any (\Bot {values=(Just a, Just b)} -> a == b) toTransfer

applyWhile cond func state =
  if(not (cond state)) then
    state
  else
    applyWhile cond func (func state)

findBotComparing states v1 v2 =
  if(exists) then botWhere else Nothing
  where
    botStates = map fst states
    exists = any (pairExists desiredPair) botStates
    Just stateWhere = find (pairExists desiredPair) botStates
    botWhere = find (checkIfMatches desiredPair) stateWhere
    desiredPair = (Just v1, Just v2)


main = do
  input <- readFile "input"
  let bots = generateBots input
  let initialState = [(bots, [], False)]
  let states = applyWhile anyHaveBoth transfer initialState
  return $ head states
