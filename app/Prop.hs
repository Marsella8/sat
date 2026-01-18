module Prop
  ( Var(..)
  , Literal(..), lit, (¬)
  , Clause(..), clause
  , CNF(..), cnf
  , NNF(..)
  , Negatable(..)
  , HasVars(..)
  , WFF(..), var, and, or, xor, not
  ) where

import Prelude hiding (and, or, not)
import qualified Data.Set as Set
import Data.List (intercalate)

class HasVars a where
  vars :: a -> Set.Set Var

class Negatable a where
  neg :: a -> a


newtype Var = Var String
  deriving (Eq, Ord)  

instance Show Var where
    show (Var s) = s

data Literal = Pos Var | Neg Var -- since we model SAT solvers, we only have variables in the literal (no true or false)
  deriving (Eq, Ord)

instance HasVars Literal where
    vars (Pos x) = Set.singleton x
    vars (Neg x) = Set.singleton x

instance Negatable Literal where
    neg (Pos x) = Neg x
    neg (Neg x) = Pos x

lit :: String -> Literal
lit = Pos . Var

instance Show Literal where
    show (Pos x) = show x
    show (Neg x) = "¬" ++ show x

infixl 9 ¬
(¬) :: Negatable a => a -> a
(¬) = neg


newtype Clause = Clause (Set.Set Literal)
  deriving (Eq, Ord)

instance Semigroup Clause where
    Clause s1 <> Clause s2 = Clause (Set.union s1 s2)

instance Show Clause where
    show (Clause s) = "(" ++ intercalate " ∨ " (map show (Set.toList s)) ++ ")"

clause :: [Literal] -> Clause
clause = Clause . Set.fromList

instance HasVars Clause where
    vars (Clause s) = foldMap vars s


newtype CNF = CNF (Set.Set Clause)
  deriving (Eq, Ord)

instance Show CNF where
    show (CNF c) = intercalate " ∧ " (map show (Set.toList c))

cnf :: [Clause] -> CNF
cnf = CNF . Set.fromList

instance HasVars CNF where
    vars (CNF s) = foldMap vars s

data NNF = NLit Literal | NOr NNF NNF | NAnd NNF NNF
  deriving (Eq, Ord)

instance Negatable NNF where
    neg (NLit l)   = NLit (neg l)
    neg (NOr a b)  = NAnd (neg a) (neg b)
    neg (NAnd a b) = NOr (neg a) (neg b)

instance Show NNF where
    show (NLit l)   = show l
    show (NOr a b)  = "(" ++ show a ++ " ∨ " ++ show b ++ ")"
    show (NAnd a b) = "(" ++ show a ++ " ∧ " ++ show b ++ ")"

-- Logic Wffs

data WFF
  = V Var
  | Not       WFF
  | Or        WFF WFF
  | And       WFF WFF
  | Xor       WFF WFF
  | Implies   WFF WFF
  | Coimplies WFF WFF
  deriving (Eq, Ord)

instance Show WFF where
  show (V x)           = show x
  show (Not a)         = "¬" ++ show a
  show (Or a b)        = "(" ++ show a ++ " ∨ " ++ show b ++ ")"
  show (And a b)       = "(" ++ show a ++ " ∧ " ++ show b ++ ")"
  show (Xor a b)       = "(" ++ show a ++ " ⊕ " ++ show b ++ ")"
  show (Implies a b)   = "(" ++ show a ++ " → " ++ show b ++ ")"
  show (Coimplies a b) = "(" ++ show a ++ " ↔ " ++ show b ++ ")"

var :: String -> WFF
var = V . Var

and :: [WFF] -> WFF
and []  = error "and: empty"
and [f] = f
and fs  = foldl1 And fs

or :: [WFF] -> WFF
or []  = error "or: empty"
or [f] = f
or fs  = foldl1 Or fs

xor :: [WFF] -> WFF
xor []  = error "xor: empty"
xor [f] = f
xor fs  = foldl1 Xor fs

not :: WFF -> WFF
not (Not a) = a
not a = Not a
