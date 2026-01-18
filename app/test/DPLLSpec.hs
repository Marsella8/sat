module DPLLSpec where
import Test.Hspec
import Prop (Literal(..), Var(..), lit, (¬), cnf, clause)
import DPLL (maybeApplyPropagate, maybeApplyPure, maybeApplyDecide, maybeApplyBacktrack, PartialAssignment, AssignedLiteral(..))

p, q, r, s, t :: Literal
p = lit "p"
q = lit "q"
r = lit "r"
s = lit "s"
t = lit "t"

spec :: Spec
spec = do
  describe "propagate" $ do
    it "propagates unit clause [p] correctly" $ do
      let c =
            cnf
              [ clause [p, (¬) q, r]
              , clause [(¬) p, r, s]
              , clause [p]
              , clause [q, (¬) r]
              ]
          input = []
          correct = [Deduced p]
      maybeApplyPropagate c input `shouldBe` Just (c, correct)

  describe "pure" $ do
    it "applies pure literal p correctly" $ do
      let c =
            cnf
              [ clause [p, q]
              , clause [p, (¬) r]
              , clause [(¬) q, r]
              ]
          input = []
          correct = [Deduced p]
      maybeApplyPure c input `shouldBe` Just (c, correct)

  describe "decide" $ do
    it "makes a decision on an unassigned variable" $ do
      let c =
            cnf
              [ clause [p, q]
              , clause [(¬) p, r]
              ]
          input = []
      case maybeApplyDecide c input of
        Just (outputCNF, pa') -> do
          outputCNF `shouldBe` c
          length pa' `shouldBe` 1
        Nothing -> fail "decide should apply"

  describe "backtrack" $ do
    it "backtracks on conflict" $ do
      let c =
            cnf
              [ clause [p]
              , clause [(¬) p]
              ]
          input = [Decided p]
          correct = [Deduced ((¬) p)]
      maybeApplyBacktrack c input `shouldBe` Just (c, correct)

main :: IO ()
main = hspec spec
