
module Memo (Memo,memo,Trie,trie,untrie) where

memo :: Memo i => (i -> o) -> (i -> o)
memo = untrie . trie

class Memo i where
  data Trie i o
  trie :: (i -> o) -> Trie i o
  untrie :: Trie i o -> (i -> o)

instance Memo Bool where
  data Trie Bool o = Two o o
  trie f = Two (f True) (f False)
  untrie (Two x y) = \b -> if b then x else y

instance Memo a => Memo [a] where
  data Trie [a] o = List o (Trie a (Trie [a] o))
  trie f = List (f []) (trie (\x -> trie (\xs -> f (x:xs))))
  untrie (List tn tc) = \case [] -> tn; x:xs -> untrie (untrie tc x) xs

instance Memo Integer where
  newtype Trie Integer o = WrapII { unwrapII :: Trie [Bool] o }
  trie f = WrapII (trie (f . unbitsII))
  untrie f = untrie (unwrapII f) . bitsII

bitsII :: Integer -> [Bool]
bitsII n = if n == 0 then [] else (n `mod` 2 == 1) : bitsII (n `div` 2)

unbitsII :: [Bool] -> Integer
unbitsII = \case [] -> 0; x:xs -> 2 * unbitsII xs + (if x then 1 else 0)
