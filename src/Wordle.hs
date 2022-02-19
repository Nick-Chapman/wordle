module Wordle (main) where

import Prelude hiding (Word)
import System.Environment (getArgs)
import qualified Data.Map.Strict as Map

main :: IO ()
main = getArgs >>= (run . parse)

parse :: [String] -> Config
parse = \case
  [] -> Play
  ["gen","entropy"] -> GenEntropy { answers = Answers, guesses = Answers }
  ["gen","entropy","all"] -> GenEntropy { answers = Answers, guesses = Legal }
  args ->
    error (show ("parse",args))

run :: Config -> IO ()
run = \case
  Play -> play
  GenEntropy {answers,guesses} -> do
    answers <- loadDD answers
    guesses <- loadDD guesses
    genEntropy answers guesses

--[config]-----------------------------------------------------------

data Config
  = Play
  | GenEntropy { answers:: DictDescriptor, guesses:: DictDescriptor }

data DictDescriptor = Answers | Legal

--[load]--------------------------------------------------------------

loadDD :: DictDescriptor -> IO Dict
loadDD = \case
  Answers -> load "answers.sorted"
  Legal -> load "legal.sorted"

load :: FilePath -> IO Dict
load path = do
  s <- readFile path
  let words = [ makeWord line | line <- lines s ]
  pure (Dict words)

--[play]-------------------------------------------------------------

play :: IO ()
play = do
    answers <- loadDD Answers
    playBot answers

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

data Dict = Dict { dictWords :: [Word] }

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

playBot :: Dict -> IO ()
playBot answers = do
  myGames <- load "my-games.list"
  let guess1 = makeWord "weary"
  let theBot = makeBot1 guess1 answers
  let _ = sequence_ [ runGame hidden theBot | hidden <- dictWords myGames ]
  putStrLn "------------------------------"
  testBotDisplay answers theBot
  putStrLn "------------------------------"
  testBotDisplay answers (makeBot1 (makeWord "raise") answers)

makeBot1 :: Word -> Dict -> Bot
makeBot1 guess1 answers = do
  let description =  "{guess1 = " ++ show guess1 ++ "; then first word remaining}"
  let
    act =
      --Log ("I am: " ++ description) $
      --Log ("#answers = " ++ show (length (dictWords answers))) $
      Guess guess1 (loop answers guess1)
  Bot { description, act }
  where
    loop :: Dict -> Word -> Mark -> Act
    loop remaining lastGuess mark = do
      let remaining' = filterDict remaining lastGuess mark
      let n = length (dictWords remaining)
      let n' = length (dictWords remaining')
      let p :: Double = fromIntegral n' / fromIntegral n
      let _e = logBase 2.0 (1.0 / p)
      let _message = "#remaining = " ++ show n' -- ++ " {e = " ++ show e ++ " }"
      let nextGuess = anyWordFromDict remaining'
      --Log _message $ Guess nextGuess (loop remaining' nextGuess)
      Guess nextGuess (loop remaining' nextGuess)

filterDict :: Dict -> Word -> Mark -> Dict
filterDict dict guess mark =
  Dict [ word
       | word <- dictWords dict
       , computeMark guess word == mark
       ]

anyWordFromDict :: Dict -> Word
anyWordFromDict Dict { dictWords } =
  if length dictWords == 0 then error "anyWordFromDict: dict is empty" else
    head dictWords

runGame :: Word -> Bot -> IO ()
runGame hidden Bot{act} = do
  putStrLn "------------------------------"
  putStrLn ("GameRunner: hidden = " ++ show hidden)
  run 1 act
  where
    run :: Int -> Act -> IO ()
    run n = \case
      Log message act -> do putStrLn ("bot: " ++ message); run n act
      Guess guess f -> do
        let mark = computeMark guess hidden
        putStrLn ("guess#" ++ show n ++ " : " ++ show guess ++ " --> " ++ show mark)
        if guess == hidden then pure () else
          run (n+1) (f mark)

-- | test bot over a set of words, returning the number of guess for each word
testBotDisplay :: Dict -> Bot -> IO ()
testBotDisplay dict Bot{description,act} = do
  let ns = [ howManyGuess hidden act | hidden <- dictWords dict ]
  let dist = [ (n,length xs) | (n,xs) <- hist [ (n,()) | n <- ns ] ]
  let tot = sum ns
  let av :: Double = fromIntegral tot / fromIntegral (length (dictWords dict))
  putStrLn (
    "test-bot...\n" ++
    "description: " ++ description ++ "\n" ++
    "#games=" ++ show (length ns) ++ "\n" ++
    --"results=" ++ show ns ++ "\n" ++
    "total=" ++ show tot ++ "\n" ++
    "max=" ++ show (maximum ns) ++ "\n" ++
    "distribution" ++ show dist ++ "\n" ++
    "average=" ++ show av)

howManyGuess :: Word -> Act -> Int
howManyGuess hidden = run 1
  where
    run :: Int -> Act -> Int
    run n = \case
      Log _ bot -> run n bot
      Guess guess f -> do
        let mark = computeMark guess hidden
        if guess == hidden then n else
          run (n+1) (f mark)
