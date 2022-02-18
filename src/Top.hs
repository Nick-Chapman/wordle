module Top (main) where

import Prelude hiding (Word)
--import Data.List (sortBy)
--import Data.Ord (comparing)

main :: IO ()
main = do
  putStrLn "*wordle*"
  dict <- loadDictionary
  print ("#dict", length $ allDictWords dict)
  explore dict (mkWord "happy")
  explore dict (mkWord "weary")
  explore dict (mkWord "spoil")
  explore dict (mkWord "slate")
  mapM_ (explore dict) (allDictWords dict)

loadDictionary :: IO Dict
loadDictionary = do
  s <- readFile "words.sorted"
  let words = [ mkWord line | line <- lines s ]
  pure (Dict words)

explore :: Dict -> Word -> IO ()
explore d w = do
  --print w
  s <- computeScore d w
  print (w,s)

computeScore :: Dict -> Word -> IO Score
computeScore dict guess = do
  let
    all =
      [ (mark,con,matching,score)
      | mark <- allMarks
      , let con = computeCon guess mark
      , let matching = [ x | x <- allDictWords dict, satCon con x ]
      , let score = mkScore dict (length matching)
      ]
    -- _sorted = dropWhile (\(_,_,_,x) -> x==Count 0) $ sortBy (comparing (\(_,_,_,x) -> x)) all
  --mapM_ print (take 100 sorted)
  let worst = _average [ s | (_,_,_,s) <- all ]
  pure worst


_average :: [Score] -> Score
_average xs = Count (sum (map countOfScore xs))

computeCon :: Word -> Mark -> Constraints
computeCon (Word wq) (Mark mq) =
  [ (if c == Green then LetInPos else LetNotInPos) l p
  | (p,(l,c)) <- listQ (zipQ thePosQuin (zipQ wq mq))
  ] ++
  [ (if hasGrey then LetCountEq else LetCountGeq) l (g+y)
  | l <- allLetters
  , let cols = [ c | (c,l') <- listQ (zipQ mq wq), l==l' ]
  , let y = length [ () | Yellow <- cols ]
  , y>=1
  , let g = length [ () | Green <- cols ]
  , let hasGrey = length [ () | Grey <- cols ] >= 1
  ]



allLetters :: [Letter]
allLetters = map Letter ['a'..'z']


type Constraints = [Con1]-- constraint

thePosQuin :: Quin Pos
thePosQuin = Quin A B C D E

satCon :: Constraints -> Word -> Bool
satCon cs w = all (satCon1 w) cs


satCon1 :: Word -> Con1 -> Bool
satCon1 w@(Word wq) = \case
  LetInPos l p -> l == getLetter p w
  LetNotInPos l p -> l /= getLetter p w
  LetCountEq l n -> length [ () | l' <- listQ wq, l==l' ] == n
  LetCountGeq l n -> length [ () | l' <- listQ wq, l==l' ] >= n


mkScore :: Dict -> Int -> Score
mkScore _dict n = Count n

allMarks :: [Mark]
allMarks =
  [ Mark (Quin a b c d e)
  | a <- col, b <- col, c <- col, d <- col, e <- col
  ]
  where col = [Green,Yellow,Grey]

{-isMarked :: Word -> Mark -> Word -> Bool
isMarked guess mark candidate =
  undefined guess mark candidate
-}

_mark :: Word -> Word -> Mark
_mark answer guess = undefined answer guess

{-
[ res
| (p,(a,g)) <- listQ (zipQ posQuin (zipQ aq gq))
, let res = if a==g then Green else ...
]
-}
{-
_foo :: Pos -> Letter -> Letter -> Colour
_foo _pos a g =
  if a==g then Green else
    undefined
    p <- positionsHavingLetter g, getLetter p answer <> g
-}

getLetter :: Pos -> Word -> Letter
getLetter p (Word q) = indexQ q p

mkWord :: String -> Word
mkWord = \case
  a:b:c:d:e:_ -> Word (fmap Letter (Quin a b c d e))
  s -> error (show ("mkWord",s))


data Con1
  = LetInPos Letter Pos
  | LetNotInPos Letter Pos
  | LetCountEq Letter Int
  | LetCountGeq Letter Int

-- number of words in dict which match
-- fraction of dict which matches -- smmaller is better
-- later will use an entropy measure (log 1/), so bigger is better
data Score = Count { countOfScore :: Int }
  deriving (Eq,Ord)

data Dict = Dict { allDictWords :: [Word] }

newtype Word = Word (Quin Letter)

newtype Mark = Mark (Quin Colour)

data Colour = Green | Yellow | Grey
  deriving (Eq)

newtype Letter = Letter { unLetter :: Char }
  deriving (Eq)

data Pos = A | B | C | D | E
  deriving Show


instance Show Word where
  show (Word q) = "word:" ++ show (listQ (fmap unLetter q))

instance Show Mark where
  show (Mark q) = "mark[" ++ concat (map show (listQ q)) ++ "]"

instance Show Colour where
  show = \case Green -> "G"; Yellow -> "y"; Grey -> "-"

instance Show Letter where
  show (Letter c) = show c

instance Show Con1 where
  show = \case
    LetInPos l p -> show p ++ "=" ++ show l
    LetNotInPos l p -> show p ++ "/=" ++ show l
    LetCountEq l n -> "#" ++ show l ++ "=" ++ show n
    LetCountGeq l n -> "#" ++ show l ++ ">=" ++ show n

instance Show Score where
  show (Count n) = "#" ++ show n

data Quin a = Quin a a a a a
  deriving (Show,Functor)

indexQ :: Quin a -> Pos -> a
indexQ (Quin a b c d e) = \case A -> a; B -> b; C -> c; D -> d; E -> e

listQ :: Quin a -> [a]
listQ (Quin a b c d e) = [a,b,c,d,e]

zipQ :: Quin a -> Quin b -> Quin (a,b)
zipQ (Quin a b c d e) (Quin v w x y z) = Quin (a,v) (b,w) (c,x) (d,y) (e,z)
