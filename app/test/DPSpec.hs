module DPSpec where
import Test.Hspec
import Prop (Literal(..), Var(..), lit, (¬), cnf, clause)
import DP (maybeApplyUnitPropagation, maybeApplyPureLiteral, maybeApplyClashingClause, maybeApplyResolution, applyResolution)

p, q, r, s, t :: Literal
p = lit "p"
q = lit "q"
r = lit "r"
s = lit "s"
t = lit "t"

spec :: Spec
spec = do
  describe "unit propagation" $ do
    it "propagates units correctly" $ do
      let input =
            cnf
              [ clause [p, (¬) q, r]
              , clause [(¬) p, (¬) q, r]
              , clause [p]
              , clause [q, (¬) r]
              ]
          correct =
            cnf
              [ clause [(¬) q, r]
              , clause [q, (¬) r]
              ]

      maybeApplyUnitPropagation input `shouldBe` Just correct

  describe "pure literal" $ do
    it "applies pure literal correctly" $ do
      let input =
            cnf
              [ clause [p, q]
              , clause [p, (¬) r]
              , clause [(¬) q, r]
              ]
          correct =
            cnf
              [ clause [(¬) q, r]
              ]

      maybeApplyPureLiteral input `shouldBe` Just correct

  describe "clashing clause" $ do
    it "removes clashing clauses correctly" $ do
      let input =
            cnf
              [ clause [p, (¬) p, q]
              , clause [q, r]
              ]
          correct =
            cnf
              [ clause [q, r]
              ]

      maybeApplyClashingClause input `shouldBe` Just correct

  describe "resolution" $ do
    it "resolves a simple case (pivot p)" $ do
      let input =
            cnf
              [ clause [p, (¬) q, r]
              , clause [(¬) p, (¬) q, r]
              ]
          correct =
            cnf
              [ clause [(¬) q, r]
              ]
      applyResolution input (Var "p") `shouldBe` correct

    it "resolves into 4 clauses (2x2) (pivot t)" $ do
      let input =
            cnf
              [ clause [t, p]
              , clause [t, q]
              , clause [(¬) t, r]
              , clause [(¬) t, s]
              ]
          correct =
            cnf
              [ clause [p, r]
              , clause [p, s]
              , clause [q, r]
              , clause [q, s]
              ]
      applyResolution input (Var "t") `shouldBe` correct

main :: IO ()
main = hspec spec
