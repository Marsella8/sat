module Asst where

import qualified Data.Map as Map
import Prop hiding (not)

type Assignment = Map.Map Var Bool

class Sat a where
    sat :: a -> Assignment -> Bool

instance Sat Literal where
    sat (Pos v) a = Map.findWithDefault False v a
    sat (Neg v) a = not (Map.findWithDefault False v a)

instance Sat Clause where
    sat (Clause s) a = any (`sat` a) s

instance Sat CNF where
    sat (CNF s) a = all (`sat` a) s
