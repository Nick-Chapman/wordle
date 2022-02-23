
module Letter (Letter, mkLetter, unLetter) where

import Memo (Memo,Trie,trie,untrie)

data Letter = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Eq,Ord)

mkLetter :: Char -> Letter
mkLetter = \case 'a' -> A; 'b' -> B; 'c' -> C; 'd' -> D; 'e' -> E; 'f' -> F; 'g' -> G; 'h' -> H; 'i' -> I; 'j' -> J; 'k' -> K; 'l' -> L; 'm' -> M; 'n' -> N; 'o' -> O; 'p' -> P; 'q' -> Q; 'r' -> R; 's' -> S; 't' -> T; 'u' -> U; 'v' -> V; 'w' -> W; 'x' -> X; 'y' -> Y; 'z' -> Z; x -> error (show ("mkLetter",x))

unLetter :: Letter -> Char
unLetter = \case A -> 'a'; B -> 'b'; C -> 'c'; D -> 'd'; E -> 'e'; F -> 'f'; G -> 'g'; H -> 'h'; I -> 'i'; J -> 'j'; K -> 'k'; L -> 'l'; M -> 'm'; N -> 'n'; O -> 'o'; P -> 'p'; Q -> 'q'; R -> 'r'; S -> 's'; T -> 't'; U -> 'u'; V -> 'v'; W -> 'w'; X -> 'x'; Y -> 'y'; Z -> 'z'

instance Memo Letter where

  data Trie Letter a = PerLetter a a a a a a a a a a a a a a a a a a a a a a a a a a

  trie x = PerLetter (x A) (x B) (x C) (x D) (x E) (x F) (x G) (x H) (x I) (x J) (x K) (x L) (x M) (x N) (x O) (x P) (x Q) (x R) (x S) (x T) (x U) (x V) (x W) (x X) (x Y) (x Z)

  untrie (PerLetter a b c d e f g h i j k l m n o p q r s t u v w x y z) = \case A -> a; B -> b; C -> c; D -> d; E -> e; F -> f; G -> g; H -> h; I -> i; J -> j; K -> k; L -> l; M -> m; N -> n; O -> o; P -> p; Q -> q; R -> r; S -> s; T -> t; U -> u; V -> v; W -> w; X -> x; Y -> y; Z -> z
