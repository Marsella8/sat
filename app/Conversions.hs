module Conversions (wffToCNF, wffToNNF, nnfToCNF) where

import Prop

wffToNNF :: WFF -> NNF
wffToNNF (V x)           = NLit (Pos x)
wffToNNF (Not f)         = neg (wffToNNF f)
wffToNNF (Or a b)        = NOr (wffToNNF a) (wffToNNF b)
wffToNNF (And a b)       = NAnd (wffToNNF a) (wffToNNF b)
wffToNNF (Implies a b)   = NOr (neg (wffToNNF a)) (wffToNNF b)
wffToNNF (Coimplies a b) = NAnd (NOr (neg ga) gb) (NOr (neg gb) ga)
  where ga = wffToNNF a; gb = wffToNNF b
wffToNNF (Xor a b)       = NAnd (NOr ga gb) (NOr (neg ga) (neg gb))
  where ga = wffToNNF a; gb = wffToNNF b

nnfToCNF :: NNF -> CNF
nnfToCNF = cnf . go
  where
    go :: NNF -> [Clause]
    go (NLit l)   = [clause [l]]
    go (NAnd a b) = go a ++ go b
    go (NOr a b)  = [c1 <> c2 | c1 <- go a, c2 <- go b] -- cartesian product!!! bad!!! tseitin addresses this

wffToCNF :: WFF -> CNF
wffToCNF = nnfToCNF . wffToNNF
