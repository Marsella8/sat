module Conversions (formulaToCNF, formulaToNNF, nnfToCNF) where

import Prop

formulaToNNF :: Formula -> NNF
formulaToNNF (V x)           = NLit (Pos x)
formulaToNNF (Not f)         = neg (formulaToNNF f)
formulaToNNF (Or a b)        = NOr (formulaToNNF a) (formulaToNNF b)
formulaToNNF (And a b)       = NAnd (formulaToNNF a) (formulaToNNF b)
formulaToNNF (Implies a b)   = NOr (neg (formulaToNNF a)) (formulaToNNF b)
formulaToNNF (Coimplies a b) = NAnd (NOr (neg ga) gb) (NOr (neg gb) ga)
  where ga = formulaToNNF a; gb = formulaToNNF b
formulaToNNF (Xor a b)       = NAnd (NOr ga gb) (NOr (neg ga) (neg gb))
  where ga = formulaToNNF a; gb = formulaToNNF b

nnfToCNF :: NNF -> CNF
nnfToCNF = cnf . go
  where
    go :: NNF -> [Clause]
    go (NLit l)   = [clause [l]]
    go (NAnd a b) = go a ++ go b
    go (NOr a b)  = [c1 <> c2 | c1 <- go a, c2 <- go b] -- cartesian product!!! bad!!! tseitin addresses this

formulaToCNF :: Formula -> CNF
formulaToCNF = nnfToCNF . formulaToNNF
