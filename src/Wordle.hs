module Wordle (main) where

import Data.List (intercalate,maximumBy,sort,sortBy)
import Data.Ord (comparing)
import Data.Set (Set)
import Prelude hiding (Word)
import System.Environment (getArgs)
import System.IO (hFlush,stdout)
import System.Random (getStdRandom,randomR)
import Text.Printf (printf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = getArgs >>= (run . parse)

parse :: [String] -> Config
parse = \case
  ["gen","entropy"] -> GenEntropy Answers
  ["gen","entropy","all"] -> GenEntropy Legal

  ["bot1"] -> TestBot Bot1
  ["bot2"] -> TestBot Bot2
  ["bot3"] -> TestBot Bot3
  ["bot4"] -> TestBot Bot4

  ["tab","bot1"] -> TabulateBot Bot1
  ["tab","bot2"] -> TabulateBot Bot2
  ["tab","bot3"] -> TabulateBot Bot3
  ["tab","bot4"] -> TabulateBot Bot4

  ["view","bot1"] -> ViewBot Bot1
  ["view","bot2"] -> ViewBot Bot2
  ["view","bot3"] -> ViewBot Bot3
  ["view","bot4"] -> ViewBot Bot4
  ["view"] -> ViewBot Bot4

  ["assist","bot1",s] -> Assist Bot1 (SelectedHidden s)
  ["assist","bot2",s] -> Assist Bot2 (SelectedHidden s)
  ["assist","bot3",s] -> Assist Bot3 (SelectedHidden s)
  ["assist","bot4",s] -> Assist Bot4 (SelectedHidden s)

  ["assist",s] -> Assist Bot4 (SelectedHidden s)
  ["assist"] -> Assist Bot4 RandomHidden

  ["play",s] -> PlayGame (SelectedHidden s)
  ["play"] -> PlayGame RandomHidden
  [] -> PlayGame RandomHidden

  args ->
    error (show ("parse",args))

--[config]-----------------------------------------------------------

data Config
  = GenEntropy DictDescriptor
  | TestBot BotDescriptor -- over all 2315 games
  | TabulateBot BotDescriptor -- over all 2315 games
  | ViewBot BotDescriptor -- over recent games
  | PlayGame Puzzle -- play game, with no assistance
  | Assist BotDescriptor Puzzle -- play game, *with* assistance

data Puzzle = SelectedHidden String | RandomHidden

data DictDescriptor = Answers | Legal

data BotDescriptor = Bot1 | Bot2 | Bot3 | Bot4

--[run]--------------------------------------------------------------

run :: Config -> IO ()
run config = do
  legal <- loadDD Legal
  answers <- loadDD Answers

  case config of
    GenEntropy dict -> do
      dict <- loadDD dict
      genEntropy answers dict

    TabulateBot bd -> do
      bot <- makeBotFromDescriptor bd
      tabulateBot legal answers bot

    TestBot bd -> do
      bot <- makeBotFromDescriptor bd
      testBot legal answers bot

    ViewBot bd -> do
      bot <- makeBotFromDescriptor bd
      myGames <- load "my-games.list"
      sequence_ [ viewGame legal answers hidden bot | hidden <- dictWords myGames ]

    PlayGame puzzle -> do
      hidden <- getPuzzleWord answers puzzle
      let player = human
      playGame legal answers hidden player

    Assist bd puzzle -> do
      hidden <- getPuzzleWord answers puzzle
      bot <- makeBotFromDescriptor bd
      let player = assistedHuman bot
      playGame legal answers hidden player


getPuzzleWord :: Dict -> Puzzle -> IO Word
getPuzzleWord answers = \case
  SelectedHidden s -> pure $ makeAnswerWord answers s
  RandomHidden -> do
    putStrLn "Selecting random word..."
    randomListPick (dictWords answers)

makeBotFromDescriptor :: BotDescriptor -> IO Bot
makeBotFromDescriptor desc = do
    answers <- loadDD Answers
    let guess1 = makeWord "raise"
    let
      mk = case desc of
        Bot1 -> makeBot1
        Bot2 -> makeBot2
        Bot3 -> makeBot3
        Bot4 -> makeBot4
    pure $ mk guess1 answers

--[load]--------------------------------------------------------------

loadDD :: DictDescriptor -> IO Dict
loadDD = \case
  Answers -> load "answers.sorted"
  Legal -> load "legal.sorted"

load :: FilePath -> IO Dict
load path = do
  s <- readFile path
  let words = [ makeWord line | line <- lines s ]
  pure (Dict (Set.fromList words))

--[entropy]-----------------------------------------------------------

genEntropy :: Dict -> Dict -> IO ()
genEntropy answers guesses = do
  mapM_ pr [ (guess, calcEntropy answers guess) | guess <- dictWords guesses ]
  pure ()
    where pr (w,d) = putStrLn (show w ++ " " ++ show d)

calcEntropy :: Dict -> Word -> Double
calcEntropy dict guess = do
  let
    sizeDict :: Double = fromIntegral (length (dictWords dict))
    h1 =
      hist [ (computeMark guess hidden,hidden) | hidden <- dictWords dict ]
  sum [ p*e
      | (_mark,words) <- h1
      , let n = length words
      , let p = fromIntegral n / sizeDict
      , let e = logBase 2.0 (1.0 / p)
      ]

--[marking]-----------------------------------------------------------

filterDict :: Dict -> Word -> Mark -> Dict
filterDict dict guess mark =
  makeDict [ word
           | word <- dictWords dict
           , computeMark guess word == mark
           ]

-- ensure guesses are within a set of legal words
computeMarkChecked :: Dict -> Word -> Word -> Maybe Mark
computeMarkChecked legal guess hidden =
  if guess `Set.notMember` (dictSet legal) then Nothing else
    Just $ computeMark guess hidden

computeMark :: Word -> Word -> Mark
computeMark (Word guess) (Word hidden) = do
  let
    pass1 :: ([Letter],(Letter,Letter)) -> (Bool,[Letter])
    pass1 (unused,(guess,hidden)) = do
      let match = (guess == hidden)
      (match,if match then unused else hidden:unused)

  let
    pass2 :: ([Letter],(Letter,Bool)) -> (Colour,[Letter])
    pass2 (unused,(guess,isGreen)) =
      if isGreen then (Green, unused) else
        case lookYellow guess unused of
          Nothing -> (Black, unused)
          Just unused -> (Yellow, unused)

  let (greenMatchQ,unused) = scanQ ([], zipQ guess hidden) pass1
  let (colourQ,_) = scanQ (unused, zipQ guess greenMatchQ) pass2

  Mark colourQ

lookYellow :: Letter -> [Letter] -> Maybe [Letter]
lookYellow x initial = loop [] initial
  where
    loop acc = \case
      [] -> Nothing
      y:ys -> if x==y then Just (acc++ys) else loop (y:acc) ys


--[types] ------------------------------------------------------------

data Dict = Dict { dictSet :: Set Word }

makeDict :: [Word] -> Dict
makeDict = Dict . Set.fromList

dictWords :: Dict -> [Word]
dictWords (Dict set) = Set.toList set

seeDictSummary :: Dict -> String
seeDictSummary dict = do
  let ws = sort (map show (dictWords dict)) -- sort not necessary
  intercalate "," $ do
    if length ws <= 5 then ws else do
      take 3 ws ++ ["..."] ++ take 2 (reverse ws)

newtype Word = Word (Quin Letter)
  deriving (Eq,Ord)

makeAnswerWord :: Dict -> String -> Word
makeAnswerWord legal s = do
  if s `Set.member` (Set.map show (dictSet legal)) then makeWord s else
    error ("not a valid answer word: " ++ s)

makeWord :: String -> Word
makeWord s = maybe (error (show ("makeWord",s))) id (tryMakeWord s)

tryMakeWord :: String -> Maybe Word
tryMakeWord = \case
  a:b:c:d:e:[] -> Just $ Word (fmap Letter (Quin a b c d e))
  _ -> Nothing

newtype Mark = Mark (Quin Colour)
  deriving (Eq,Ord)

data Colour = Green | Yellow | Black
  deriving (Eq,Ord)

newtype Letter = Letter { unLetter :: Char }
  deriving (Eq,Ord)

data Pos = A | B | C | D | E
  deriving Show

--[Show] -------------------------------------------------------------

instance Show Word where
  show (Word q) = listQ (fmap unLetter q)

instance Show Mark where
  show (Mark q) = "[" ++ concat (map show (listQ q)) ++ "]"

instance Show Colour where
  show = \case Green -> "G"; Yellow -> "y"; Black -> "-"

instance Show Letter where
  show (Letter c) = show c

--[Quin]--------------------------------------------------------------

data Quin a = Quin a a a a a
  deriving (Eq,Ord,Show,Functor)

listQ :: Quin a -> [a]
listQ (Quin a b c d e) = [a,b,c,d,e]

zipQ :: Quin a -> Quin b -> Quin (a,b)
zipQ (Quin a b c d e) (Quin v w x y z) = Quin (a,v) (b,w) (c,x) (d,y) (e,z)

scanQ :: (c, Quin a) -> ((c, a) -> (b, c)) -> (Quin b, c)
scanQ (c0, Quin a1 a2 a3 a4 a5) f = (Quin b1 b2 b3 b4 b5, c5)
  where
    (b1,c1) = f (c0,a1)
    (b2,c2) = f (c1,a2)
    (b3,c3) = f (c2,a3)
    (b4,c4) = f (c3,a4)
    (b5,c5) = f (c4,a5)

--[util] -------------------------------------------------------------

hist :: Ord k => [(k,v)] -> [(k,[v])]
hist kvs = Map.toList $ Map.fromListWith (++) [ (k,[v]) | (k,v) <- kvs ]

randomListPick :: [a] -> IO a
randomListPick xs = do
  n <- getStdRandom (randomR (0,length xs - 1))
  return (xs !! n)

--[Bot]---------------------------------------------------------------

data Bot = Bot { name :: String, description :: String, act :: Act }

data Act -- Bot actions
  = Log String Act
  | Guess Word (Word -> Mark -> Act)
  | Interactive (IO Act)

--[human]-------------------------------------------------------------

human :: Bot
human = do
  let name = "human"
  let description = "real human player"
  Bot { name, description, act }
  where
    act = Interactive $ do
      guess <- readGuess
      pure $ Guess guess (\_guess _mark -> act)

readGuess :: IO Word
readGuess = do
  putStr "Enter guess> "
  hFlush stdout
  s <- getLine
  case tryMakeWord s of
    Just w -> pure w
    Nothing -> do
      putStrLn "bad word; a word must have 5 letters"
      readGuess

--[human assist]------------------------------------------------------

assistedHuman :: Bot -> Bot
assistedHuman Bot{name=assistName, act=assist0} = do
  let name = "assisted human"
  let description = "human assisted by: " ++ assistName
  Bot { name, description, act }
  where
    act :: Act
    act = loop assist0

    loop :: Act -> Act
    loop assist =
      case assist of
        Interactive{} -> undefined
        Log mes assist -> do
          Log ("assist: " ++ mes) $
            loop assist
        Guess g f -> do
          Log ("assist-would-guess: " ++ show g) $ do
            Interactive $ do
              guess <- readGuessDefault g
              pure $ Guess guess $ \guess mark -> do
                let assist = f guess mark
                loop assist

readGuessDefault :: Word -> IO Word
readGuessDefault def = do
  putStr "Enter guess> "
  hFlush stdout
  s <- getLine
  if s == "" then pure def else
    case tryMakeWord s of
      Just w -> pure w
      Nothing -> do
        putStrLn "bad word; a word must have 5 letters"
        readGuess

----------------------------------------------------------------------

playGame :: Dict -> Dict -> Word -> Bot -> IO ()
playGame legal answers hidden Bot{description,act} = do
  --putStrLn ("[hidden = " ++ show hidden ++ "]")
  putStrLn ("Player: " ++ description)
  run answers 1 act
  where
    run :: Dict -> Int -> Act -> IO ()
    run possible i = \case
      Interactive io -> do
        act <- io
        run possible i act
      Log message act -> do
        putStrLn message
        run possible i act
      Guess guess f -> do
        case computeMarkChecked legal guess hidden of
          Nothing -> do
            putStrLn $
              "illegal word: '" ++ show guess ++ "'"
            run possible i act
          Just mark -> do
            let n = length (dictWords possible)
            let possible' = filterDict possible guess mark
            let n' = length (dictWords possible')
            putStrLn $
              "guess #" ++ show i ++ " : " ++
              show guess ++ " --> " ++ show mark ++
              " " ++ show n ++ "/" ++ show n'
            putStrLn ("possible: " ++ seeDictSummary possible')
            if guess == hidden || i==25 then pure () else
              run possible' (i+1) (f guess mark)

--[view]--------------------------------------------------------------

viewGame :: Dict -> Dict -> Word -> Bot -> IO ()
viewGame legal answers hidden Bot{act} = do
  putStrLn "------------------------------"
  putStrLn ("ViewGame: hidden = " ++ show hidden)
  run answers 1 act
  where
    run :: Dict -> Int -> Act -> IO ()
    run possible i = \case
      Interactive{} ->
        undefined
      Log message act -> do
        putStrLn message
        run possible i act
      Guess guess f -> do
        case computeMarkChecked legal guess hidden of
          Nothing -> do
            putStrLn $
              "illegal word: '" ++ show guess ++ "'"
            run possible i act
          Just mark -> do
            let n = length (dictWords possible)
            let possible' = filterDict possible guess mark
            let n' = length (dictWords possible')
            putStrLn $
              "guess #" ++ show i ++ " : " ++
              show guess ++ " --> " ++ show mark ++
              " " ++ show n ++ "/" ++ show n'
            if guess == hidden || i==25 then pure () else
              run possible' (i+1) (f guess mark)

--[testBot]-----------------------------------------------------------

testBot :: Dict -> Dict -> Bot -> IO ()
testBot legal answers Bot{description,act} = do
  let
    loop :: [Int] -> Int -> [Word] -> IO [Int]
    loop acc i = \case
      [] -> pure acc
      hidden:more -> do
        putStr (if i `mod` 100 == 15 then [['c'..'z'] !! (i `div` 100)] else ".")
        hFlush stdout
        let! n = howManyGuess hidden act
        loop (n:acc) (i+1) more

  ns <- loop [] 1 (dictWords answers)
  putStrLn ""
  printStats description ns
  where
    howManyGuess :: Word -> Act -> Int
    howManyGuess hidden = run 1
      where
        run :: Int -> Act -> Int
        run n = \case
          Interactive{} -> error "cant test interactive bot"
          Log _ bot -> run n bot
          Guess guess f -> do
            case computeMarkChecked legal guess hidden of
              Nothing ->
                error ("testBot, illegal word: " ++ show guess)
              Just mark ->
                if guess == hidden || n == 100 then n else
                  run (n+1) (f guess mark)

--[tabulate]----------------------------------------------------------

tabulateBot :: Dict -> Dict -> Bot -> IO ()
tabulateBot legal answers Bot{description,act} = do
  let
    loop :: [Int] -> Int -> [Word] -> IO [Int]
    loop acc i = \case
      [] -> pure acc
      hidden:more -> do
        let guesses = whatGuesses hidden act
        putStrLn (intercalate "," (map show guesses))
        hFlush stdout
        let! n = length guesses
        loop (n:acc) (i+1) more

  ns <- loop [] 1 (dictWords answers)
  printStats description ns
  where
    whatGuesses :: Word -> Act -> [Word]
    whatGuesses hidden = run [] 1
      where
        run :: [Word] -> Int -> Act -> [Word]
        run acc n = \case
          Interactive{} -> error "cant tabulate interactive bot"
          Log _ bot -> run acc n bot
          Guess guess f -> do
            case computeMarkChecked legal guess hidden of
              Nothing ->
                error ("tabulateBot, illegal word: " ++ show guess)
              Just mark ->
                if guess == hidden || n == 100 then reverse (guess:acc) else
                  run (guess:acc) (n+1) (f guess mark)

printStats :: String -> [Int] -> IO ()
printStats description ns = do
  let dist = [ (n,length xs) | (n,xs) <- hist [ (n,()) | n <- ns ] ]
  let tot = sum ns
  let av :: Double = fromIntegral tot / fromIntegral (length ns)
  putStrLn (
    "\n" ++
    "description: " ++ description ++ "\n" ++
    "#games=" ++ show (length ns) ++ "\n" ++
    "total=" ++ show tot ++ "\n" ++
    "max=" ++ show (maximum ns) ++ "\n" ++
    "distribution" ++ show dist ++ "\n" ++
    "average=" ++ show av)

--[bot1]--------------------------------------------------------------

makeBot1 :: Word -> Dict -> Bot
makeBot1 guess1 answers = do
  let
    name = "bot1"
    description =
      "guess1='" ++ show guess1 ++ "'; " ++
      "choose first word from remaining"
  let act = Guess guess1 (loop answers)
  Bot { name, description, act }
  where
    loop :: Dict -> Word -> Mark -> Act
    loop remaining lastGuess mark = do
      let remaining' = filterDict remaining lastGuess mark
      let nextGuess = choose remaining'
      Guess nextGuess (loop remaining')

    choose :: Dict -> Word
    choose dict =
      if length (dictWords dict) == 0 then error "bot2: dict is empty" else
        head (dictWords dict)

--[bot2]--------------------------------------------------------------

makeBot2 :: Word -> Dict -> Bot
makeBot2 guess1 answers = do
  let
    name = "bot2"
    description =
      "guess1='" ++ show guess1 ++ "'; " ++
      "choose from remaining, maximizing entropy over remaining"
  let act = Guess guess1 (loop answers)
  Bot { name, description, act }
  where
    loop :: Dict -> Word -> Mark -> Act
    loop remaining lastGuess mark = do
      let remaining' = filterDict remaining lastGuess mark
      let n = length (dictWords remaining)
      let n' = length (dictWords remaining')
      let p :: Double = fromIntegral n' / fromIntegral n
      let e = logBase 2.0 (1.0 / p)
      let
        _mes1 =
          "#remaining = " ++ show n' ++
          ", actual-entropy(" ++ show lastGuess ++ ") = " ++ show e
      let (nextGuess,ee) = choose remaining'
      let _mes2 = "calc-entropy(" ++ show nextGuess ++ ") = " ++ show ee
      --Log _mes1 $ Log _mes2 $ Guess nextGuess (loop remaining' nextGuess)
      Guess nextGuess (loop remaining')

    choose :: Dict -> (Word,Double)
    choose remaining =
      maximumBy (comparing snd)
      [ (guess, calcEntropy remaining guess) | guess <- dictWords remaining ]

--[bot3]--------------------------------------------------------------

makeBot3 :: Word -> Dict -> Bot
makeBot3 guess1 answers = do
  let
    name = "bot3"
    description =
      "guess1='" ++ show guess1 ++ "'; " ++
      "choose from answers, maximizing entropy over remaining"
  let act = Guess guess1 (loop answers)
  Bot { name, description, act }
  where
    loop :: Dict -> Word -> Mark -> Act
    loop lastRemaining lastGuess mark = do
      let remaining = filterDict lastRemaining lastGuess mark

      let
        rankedChoices = reverse $
          sortBy (comparing snd)
          [ (guess, calcEntropy remaining guess) | guess <- dictWords answers ]

      Log (showRankedChoices remaining (take 5 rankedChoices)) $ do
      let
        guess = do
          let n = length (dictWords remaining)
          if n == 1 || n==2 then head (dictWords remaining) else
            fst (head rankedChoices)

      Guess guess (loop remaining)


showRankedChoices :: Dict -> [(Word,Double)] -> String
showRankedChoices remaining wes = do
  intercalate ", " $
    [ maybeStar w ++ show w ++ "(" ++ showE e ++ ")" | (w,e) <- wes ]
  where
    maybeStar w =
      if w `Set.member` dictSet remaining then "*" else ""

showE :: Double -> String
showE = printf "%0.2f"


--[bot4]--------------------------------------------------------------

makeBot4 :: Word -> Dict -> Bot
makeBot4 guess1 answers = do
  let
    name = "bot4"
    description =
      "guess1='" ++ show guess1 ++ "'; " ++
      "choose from answers, maximizing entropy over remaining; " ++
      "prefer possible words"
  let act = Guess guess1 (loop answers)
  Bot { name, description, act }
  where
    loop :: Dict -> Word -> Mark -> Act
    loop lastRemaining lastGuess mark = do
      let remaining = filterDict lastRemaining lastGuess mark

      let
        rankedChoices = reverse $
          sortBy (comparing snd)
          [ (guess, score4 remaining guess) | guess <- dictWords answers ]

      Log (showRankedChoices remaining (take 5 rankedChoices)) $ do
      let
        guess = do
          let n = length (dictWords remaining)
          if n == 1 || n==2 then head (dictWords remaining) else
            fst (head rankedChoices)

      Guess guess (loop remaining)


score4 :: Dict -> Word -> Double
score4 remaining guess = do
  let n = length (dictWords remaining)
  let e = calcEntropy remaining guess
  let
    x = if guess `Set.notMember` dictSet remaining then 0.0 else do
      let p = 1.0 / fromIntegral n
      p * logBase 2.0 (fromIntegral n)
  e+x

{-showRankedChoicesEEE :: Dict -> [(Word,EEE)] -> String
showRankedChoicesEEE remaining xs = do
  intercalate ", " $
    [ maybeStar w ++ show w ++
      "(" ++ showE e1 ++ "+" ++ showE e2 ++ "=" ++ showE e3 ++ ")"
    | (w,(e1,e2,e3)) <- xs ]
  where
    maybeStar w =
      if w `Set.member` dictSet remaining then "*" else ""-}


----------------------------------------------------------------------

-- TODO: GameMaster variants: absurdle, interactive
-- TODO: bot4: rank works by expected entropy remaining (prefer possible!)
-- TODO: precompute/memoize marking function
-- TODO: allow bots to choose from all legal words
