module Wordle (main) where

import Data.List (intercalate,maximumBy)
import Data.Ord (comparing)
import Data.Set (Set)
import Prelude hiding (Word)
import System.Environment (getArgs)
import System.IO (hFlush,stdout)
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

  ["tab","bot1"] -> TabulateBot Bot1
  ["tab","bot2"] -> TabulateBot Bot2
  ["tab","bot3"] -> TabulateBot Bot3

  ["view","bot1"] -> ViewBot Bot1
  ["view","bot2"] -> ViewBot Bot2
  ["view","bot3"] -> ViewBot Bot3
  ["view"] -> ViewBot Bot3

  [] -> TestBot Bot1

  args ->
    error (show ("parse",args))

--[config]-----------------------------------------------------------

data Config
  = GenEntropy DictDescriptor
  | TestBot BotDescriptor -- over all 2315 games
  | TabulateBot BotDescriptor -- over all 2315 games
  | ViewBot BotDescriptor -- over recent games

data DictDescriptor = Answers | Legal

data BotDescriptor = Bot1 | Bot2 | Bot3

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
      sequence_ [ runGame legal answers hidden bot | hidden <- dictWords myGames ]


makeBotFromDescriptor :: BotDescriptor -> IO Bot
makeBotFromDescriptor desc = do
    answers <- loadDD Answers
    let guess1 = makeWord "raise"
    let mk = case desc of Bot1 -> makeBot1; Bot2 -> makeBot2; Bot3 -> makeBot3
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

newtype Word = Word (Quin Letter)
  deriving (Eq,Ord)

makeWord :: String -> Word
makeWord = \case
  a:b:c:d:e:_ -> Word (fmap Letter (Quin a b c d e))
  s -> error (show ("makeWord",s))

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

--[Bot]---------------------------------------------------------------

data Bot = Bot { description :: String, act :: Act }

data Act -- Bot actions
  = Log String Act
  | Guess Word (Mark -> Act)

runGame :: Dict -> Dict -> Word -> Bot -> IO ()
runGame legal answers hidden Bot{act} = do
  putStrLn "------------------------------"
  putStrLn ("GameRunner: hidden = " ++ show hidden)
  run answers 1 act
  where
    run :: Dict -> Int -> Act -> IO ()
    run possible i = \case
      Log message act -> do
        putStrLn ("bot: " ++ message)
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
              run possible' (i+1) (f mark)

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
          Log _ bot -> run n bot
          Guess guess f -> do
            case computeMarkChecked legal guess hidden of
              Nothing ->
                error ("testBot, illegal word: " ++ show guess)
              Just mark ->
                if guess == hidden || n == 100 then n else
                  run (n+1) (f mark)

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
          Log _ bot -> run acc n bot
          Guess guess f -> do
            case computeMarkChecked legal guess hidden of
              Nothing ->
                error ("tabulateBot, illegal word: " ++ show guess)
              Just mark ->
                if guess == hidden || n == 100 then reverse (guess:acc) else
                  run (guess:acc) (n+1) (f mark)

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
    description =
      "guess1='" ++ show guess1 ++ "'; " ++
      "choose first word from remaining"
  let act = Guess guess1 (loop answers guess1)
  Bot { description, act }
  where
    loop :: Dict -> Word -> Mark -> Act
    loop remaining lastGuess mark = do
      let remaining' = filterDict remaining lastGuess mark
      let nextGuess = choose remaining'
      Guess nextGuess (loop remaining' nextGuess)

    choose :: Dict -> Word
    choose dict =
      if length (dictWords dict) == 0 then error "bot2: dict is empty" else
        head (dictWords dict)

--[bot2]--------------------------------------------------------------

makeBot2 :: Word -> Dict -> Bot
makeBot2 guess1 answers = do
  let
    description =
      "guess1='" ++ show guess1 ++ "'; " ++
      "choose from remaining, maximizing entropy over remaining"
  let act = Guess guess1 (loop answers guess1)
  Bot { description, act }
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
      Guess nextGuess (loop remaining' nextGuess)

    choose :: Dict -> (Word,Double)
    choose remaining =
      maximumBy (comparing snd)
      [ (guess, calcEntropy remaining guess) | guess <- dictWords remaining ]

--[bot3]--------------------------------------------------------------

makeBot3 :: Word -> Dict -> Bot
makeBot3 guess1 answers = do
  let
    description =
      "guess1='" ++ show guess1 ++ "'; " ++
      "choose from answers, maximizing entropy over remaining"
  let act = Guess guess1 (loop answers guess1)
  Bot { description, act }
  where
    loop :: Dict -> Word -> Mark -> Act
    loop remaining lastGuess mark = do
      let remaining' = filterDict remaining lastGuess mark
      let nextGuess = choose remaining'
      Guess nextGuess (loop remaining' nextGuess)

    choose :: Dict -> Word
    choose remaining = do
      let n = length (dictWords remaining)
      if n == 1 || n==2 then head (dictWords remaining) else
        fst $ maximumBy (comparing snd)
        [ (guess, calcEntropy remaining guess) | guess <- dictWords answers ]

----------------------------------------------------------------------

-- TODO: interactive bot -- i.e. let a human play, with assistance
-- TODO: GameMaster variants: fixed word, random word, absurdle, interactive
-- TODO: bot4: rank works by expected entropy remaining (prefer possible!)
-- TODO: precompute/memoize marking function
-- TODO: allow bots to choose from all legal words
