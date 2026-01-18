module DPLL where

import Prop
import DP (unitPropagationApplies, pureLiteralApplies)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Data.Foldable (asum)

data AssignedLiteral = Deduced Literal | Decided Literal
  deriving (Eq, Show)

asLiteral :: AssignedLiteral -> Literal
asLiteral (Deduced l) = l
asLiteral (Decided l) = l

type PartialAssignment = [AssignedLiteral]

-- utility function, simplifies the CNF by applying the (partial) assignment
applyPartialAssignment :: CNF -> PartialAssignment -> CNF
applyPartialAssignment (CNF c) pa =
    let trues = Set.fromList [asLiteral l | l <- pa]
        falses = Set.map neg trues
    in cnf (mapMaybe (\(Clause s) ->
        if any (`Set.member` s) trues
        then Nothing
        else Just (Clause (Set.filter (`Set.notMember` falses) s))
    ) (Set.toList c))


propagateApplies :: CNF -> PartialAssignment -> Maybe Literal
propagateApplies c pa = unitPropagationApplies (applyPartialAssignment c pa)

applyPropagate :: CNF -> PartialAssignment -> Literal -> (CNF, PartialAssignment)
applyPropagate c pa l = (c, Deduced l : pa)

maybeApplyPropagate :: CNF -> PartialAssignment -> Maybe (CNF, PartialAssignment)
maybeApplyPropagate c pa = applyPropagate c pa <$> propagateApplies c pa

pureApplies :: CNF -> PartialAssignment -> Maybe Literal
pureApplies c pa = pureLiteralApplies (applyPartialAssignment c pa)

applyPure :: CNF -> PartialAssignment -> Literal -> (CNF, PartialAssignment)
applyPure c pa l = (c, Deduced l : pa)
maybeApplyPure :: CNF -> PartialAssignment -> Maybe (CNF, PartialAssignment)
maybeApplyPure c pa = applyPure c pa <$> pureApplies c pa

backtrackApplies :: CNF -> PartialAssignment -> Maybe AssignedLiteral
backtrackApplies c pa = 
    let (CNF c') = applyPartialAssignment c pa
        unsat = any (\(Clause s) -> null s) (Set.toList c')
    in if unsat then
        Just (Decided (head [l | Decided l <- pa]))  -- most recent decision (list is newest-first)
    else
        Nothing

applyBacktrack :: CNF -> PartialAssignment -> AssignedLiteral -> (CNF, PartialAssignment)
applyBacktrack c pa l = 
    let m' = tail (dropWhile (/= l) pa)  -- drop everything up to and including the decision
    in (c, Deduced (neg (asLiteral l)) : m')

maybeApplyBacktrack :: CNF -> PartialAssignment -> Maybe (CNF, PartialAssignment)
maybeApplyBacktrack c pa = applyBacktrack c pa <$> backtrackApplies c pa

decideApplies :: CNF -> PartialAssignment -> Maybe AssignedLiteral
decideApplies c pa =
    case Set.lookupMin (vars (applyPartialAssignment c pa)) of
        Just v -> Just (Decided (Pos v))
        Nothing -> Nothing

applyDecide :: CNF -> PartialAssignment -> AssignedLiteral -> (CNF, PartialAssignment)
applyDecide c pa l = (c, l : pa)

maybeApplyDecide :: CNF -> PartialAssignment -> Maybe (CNF, PartialAssignment)
maybeApplyDecide c pa = applyDecide c pa <$> decideApplies c pa

dpllSAT :: CNF -> PartialAssignment -> Maybe Bool
dpllSAT c pa = 
    let (CNF c') = applyPartialAssignment c pa
        hasEmptyClause = any (\(Clause s) -> null s) (Set.toList c')
        hasDecisions = any isDecided pa
        isDecided (Decided _) = True
        isDecided _ = False
    in if null c' then
        Just True
    else if hasEmptyClause && Prelude.not hasDecisions then
        Just False 
    else
        Nothing

dpllSingleStep :: (CNF, PartialAssignment) -> Maybe (CNF, PartialAssignment)
dpllSingleStep (c, pa) = asum [f c pa | f <- [maybeApplyPropagate, maybeApplyPure, maybeApplyBacktrack, maybeApplyDecide]]

dpllAssigned :: (CNF, PartialAssignment) -> Maybe [Literal]
dpllAssigned (c, pa) = case dpllSAT c pa of
    Just result -> if result then Just [asLiteral l | l <- pa] else Nothing
    Nothing -> case dpllSingleStep (c, pa) of
        Just result -> dpllAssigned result
        Nothing -> error "DPLL terminated but cannot determine SAT/UNSAT! Should not happen."


dpll :: CNF -> Bool
dpll c = case dpllAssigned (c, []) of
    Just _ -> True
    Nothing -> False