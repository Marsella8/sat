module DP where
import Data.Maybe (mapMaybe)
import Prop
import qualified Data.Set as Set
import Data.Foldable (asum)

-- Davis-Putnam Algorithm


--- Unit Propagation Rule

-- either it cannot be applied or we can apply it to some variable
unitPropagationApplies :: CNF -> Maybe Literal
unitPropagationApplies (CNF c) = foldr (
    \(Clause s) acc -> case acc of
        Nothing -> if Set.size s == 1 then
            Just (Set.findMin s)
            else Nothing
        Just x -> Just x
    ) Nothing c

applyUnitPropagation :: CNF-> Literal-> CNF
applyUnitPropagation (CNF c) p = cnf (mapMaybe
    (\(Clause s) ->
        if Set.member p s then
            Nothing
        else if Set.member (neg p) s then
            Just (Clause (Set.delete (neg p) s))
        else
            Just (Clause s)
    ) (Set.toList c)
  )

maybeApplyUnitPropagation :: CNF -> Maybe CNF
maybeApplyUnitPropagation c = applyUnitPropagation c <$> unitPropagationApplies c

pureLiteralApplies :: CNF -> Maybe Literal
pureLiteralApplies (CNF c) = Set.foldr (
    \v acc -> case acc of
        Just l -> Just l
        Nothing ->
            let occurrences = mapMaybe (
                    \(Clause s) ->
                        if Set.member (Pos v) s then Just (Pos v)
                        else if Set.member (Neg v) s then Just (Neg v)
                        else Nothing
                    ) (Set.toList c)
            in if Set.size (Set.fromList occurrences) == 1
               then Just (head occurrences)
               else Nothing
    ) Nothing (vars (CNF c))

applyPureLiteral :: CNF -> Literal -> CNF
applyPureLiteral (CNF c) l = cnf (mapMaybe (\(Clause s) -> if Set.member l s then Nothing else Just (Clause s)) (Set.toList c))

maybeApplyPureLiteral :: CNF -> Maybe CNF
maybeApplyPureLiteral c = applyPureLiteral c <$> pureLiteralApplies c

clashingClauseApplies :: CNF -> Maybe Clause
clashingClauseApplies (CNF c) = foldr (
    \cl acc -> case acc of
        Nothing -> if isClashing cl then Just cl else Nothing
        Just x -> Just x
    ) Nothing c
  where
    isClashing (Clause s) = any (\l -> Set.member (neg l) s) (Set.toList s)

applyClashingClause :: CNF -> Clause -> CNF
applyClashingClause (CNF c) cl = CNF (Set.delete cl c)

maybeApplyClashingClause :: CNF -> Maybe CNF
maybeApplyClashingClause c = applyClashingClause c <$> clashingClauseApplies c

-- this can only fail if all our clauses are empty.
resolutionApplies :: CNF -> Maybe Var
resolutionApplies (CNF c) = foldr (
    \(Clause s) acc -> case acc of
        Nothing -> case Set.lookupMin s of
            Just (Pos v) -> Just v
            Just (Neg v) -> Just v
            Nothing -> Nothing
        Just x -> Just x
  ) Nothing c

applyResolution :: CNF -> Var -> CNF
applyResolution (CNF c) v =
    let res p (Clause c1) (Clause c2) = Clause (Set.delete (Pos p) c1 `Set.union` Set.delete (Neg p) c2)
        select p = Set.filter (\(Clause s) -> Set.member p s) c
        posSet = select (Pos v)
        negSet = select (Neg v)
        additional = [res v c1 c2 | c1 <- Set.toList posSet, c2 <- Set.toList negSet]
        remaining = Set.toList (Set.difference c (Set.union posSet negSet))
    in cnf (additional ++ remaining)

maybeApplyResolution :: CNF -> Maybe CNF
maybeApplyResolution c = applyResolution c <$> resolutionApplies c

singleStepDP :: CNF -> Maybe CNF
singleStepDP c = asum [f c | f <- [maybeApplyClashingClause, maybeApplyUnitPropagation, maybeApplyPureLiteral, maybeApplyResolution]]

dpSAT :: CNF -> Maybe Bool
dpSAT (CNF c) = 
    let sat = null c
        unsat = any (\(Clause s) -> null s) c
    in if sat then
        Just True
    else if unsat then
        Just False
    else
        Nothing

dp :: CNF -> Bool
dp c = case dpSAT c of
    Just result -> result
    Nothing -> case singleStepDP c of
        Just c' -> dp c'
        Nothing -> error "DP terminated but cannot determine SAT/UNSAT! Should not happen."