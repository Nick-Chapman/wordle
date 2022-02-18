module Top (main) where

import Prelude hiding (Word)
import qualified Data.Map.Strict as Map

--[main]--------------------------------------------------------------------

main :: IO ()
main = do
  dict <- load "answers.sorted"
  mapM_ pr [ (word,calcEntropy dict word) | word <- allDictWords dict ]
  pure ()
    where pr (w,d) = putStrLn (show w ++ " " ++ show d)

calcEntropy :: Dict -> Word -> Double
calcEntropy answers guess = do
  let
    sizeDict :: Double = fromIntegral (length (allDictWords answers))
    h1 =
      hist [ (mark guess hidden,hidden) | hidden <- allDictWords answers ]
  sum [ p*e
      | (_mark,words) <- h1
      , let n = length words
      , let p = fromIntegral n / sizeDict
      , let e = logBase 2.0 (1.0 / p)
      ]

--[util] --------------------------------------------------------------------

hist :: Ord k => [(k,v)] -> [(k,[v])]
hist kvs = Map.toList $ Map.fromListWith (++) [ (k,[v]) | (k,v) <- kvs ]

--[types] --------------------------------------------------------------------

data Dict = Dict { allDictWords :: [Word] }

newtype Word = Word (Quin Letter)
  deriving (Eq,Ord)

mkWord :: String -> Word
mkWord = \case
  a:b:c:d:e:_ -> Word (fmap Letter (Quin a b c d e))
  s -> error (show ("mkWord",s))

newtype Mark = Mark (Quin Colour)
  deriving (Eq,Ord)

data Colour = Green | Yellow | Black
  deriving (Eq,Ord)

newtype Letter = Letter { unLetter :: Char }
  deriving (Eq,Ord)

allLetters :: [Letter]
allLetters = map Letter ['a'..'z']

data Pos = A | B | C | D | E
  deriving Show

data Quin a = Quin a a a a a
  deriving (Eq,Ord,Show,Functor)

thePosQuin :: Quin Pos
thePosQuin = Quin A B C D E

indexQ :: Quin a -> Pos -> a
indexQ (Quin a b c d e) = \case A -> a; B -> b; C -> c; D -> d; E -> e

listQ :: Quin a -> [a]
listQ (Quin a b c d e) = [a,b,c,d,e]

zipQ :: Quin a -> Quin b -> Quin (a,b)
zipQ (Quin a b c d e) (Quin v w x y z) = Quin (a,v) (b,w) (c,x) (d,y) (e,z)

data Con1
  = LetInPos Letter Pos
  | LetNotInPos Letter Pos
  | LetCountEq Letter Int
  | LetCountGeq Letter Int

--[show] --------------------------------------------------------------------

instance Show Word where
  show (Word q) = listQ (fmap unLetter q)

instance Show Mark where
  show (Mark q) = "[" ++ concat (map show (listQ q)) ++ "]"

instance Show Colour where
  show = \case Green -> "G"; Yellow -> "y"; Black -> "-"

instance Show Letter where
  show (Letter c) = show c

instance Show Con1 where
  show = \case
    LetInPos l p -> show p ++ "=" ++ show l
    LetNotInPos l p -> show p ++ "/=" ++ show l
    LetCountEq l n -> "#" ++ show l ++ "=" ++ show n
    LetCountGeq l n -> "#" ++ show l ++ ">=" ++ show n

--[load] --------------------------------------------------------------------

load :: FilePath -> IO Dict
load path = do
  s <- readFile path
  let words = [ mkWord line | line <- lines s ]
  pure (Dict words)

--[marking] --------------------------------------------------------------------

mark :: Word -> Word -> Mark
mark guess hidden =
  head [ mark | mark <- allMarks, isMarked guess mark hidden ]

allMarks :: [Mark]
allMarks =
  [ Mark (Quin a b c d e) | a <- col, b <- col, c <- col, d <- col, e <- col]
  where col = [Green,Yellow,Black]

isMarked :: Word -> Mark -> Word -> Bool
isMarked guess mark hidden = satCon (computeCon guess mark) hidden

satCon :: [Con1] -> Word -> Bool
satCon cs w = all (satCon1 w) cs

satCon1 :: Word -> Con1 -> Bool
satCon1 w@(Word wq) = \case
  LetInPos l p -> l == getLetter p w
  LetNotInPos l p -> l /= getLetter p w
  LetCountEq l n -> length [ () | l' <- listQ wq, l==l' ] == n
  LetCountGeq l n -> length [ () | l' <- listQ wq, l==l' ] >= n

getLetter :: Pos -> Word -> Letter
getLetter p (Word q) = indexQ q p

computeCon :: Word -> Mark -> [Con1]
computeCon (Word wq) (Mark mq) =
  [ (if c == Green then LetInPos else LetNotInPos) l p
  | (p,(l,c)) <- listQ (zipQ thePosQuin (zipQ wq mq))
  ] ++
  [ (if hasBlack then LetCountEq else LetCountGeq) l (g+y)
  | l <- allLetters
  , let cols = [ c | (c,l') <- listQ (zipQ mq wq), l==l' ]
  , let y = length [ () | Yellow <- cols ]
  , let hasBlack = length [ () | Black <- cols ] >= 1
  , hasBlack || y>=1
  , let g = length [ () | Green <- cols ]
  ]
