module Enumerate (enumerate) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Prop
import Asst

enumerate :: CNF -> Bool
enumerate c = enumerate' c Map.empty (Set.toList (vars c))

enumerate' :: CNF -> Assignment -> [Var] -> Bool
enumerate' c a [] = sat c a
enumerate' c a (v:vs) = enumerate' c (Map.insert v True a) vs || enumerate' c (Map.insert v False a) vs
