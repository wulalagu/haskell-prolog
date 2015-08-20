module PrologSolver(
	solve
)where

import PrologParser
import System.Environment
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Debug.Trace


solve :: [[Term]] -> [Term] -> (Bool,[Map.Map String Term])
solve program goals = 
	let (result,unification,_) = solveWithUnification program goals Map.empty 0
	in (result,map (extractVariable (getVariables goals)) unification) 
--    in (result,unification) 

extractVariable :: [String] -> Map.Map String Term -> Map.Map String Term
extractVariable [] _ = Map.empty
extractVariable (x:xs) unification = 
	case Map.lookup x unification of
		Nothing -> extractVariable xs unification
		Just t  -> Map.insert x t $ extractVariable xs unification


solveWithUnification :: [[Term]] -> [Term] -> Map.Map String Term ->Int -> (Bool,[Map.Map String Term],Int)
solveWithUnification _ [] oldUnification seq = (True,[oldUnification],seq)
solveWithUnification program (x:xs) oldUnification seq = 
	let (ps,newSeq) = initRules (filter (isRelevant x) program) seq
	    results = map  (matchPredicate x xs oldUnification) ps 
	in combineSolution [solveWithUnification program newGoals unification newSeq |(isMatch,newGoals,unification)<-results,isMatch]

getVariables :: [Term] -> [String]
getVariables [] = []
getVariables (Variable x :xs) = (x : (getVariables xs))
getVariables (Struct _ _ params : xs) = (getVariables params) ++ (getVariables xs)

isRelevant :: Term -> [Term] -> Bool

isRelevant (Struct name paramNum params)  ((Struct n pn _):conditions) 
	| (n == name) && (pn == paramNum) = True 
	| otherwise = False

isRelevant _ _ = False

matchPredicate :: Term -> [Term]  ->Map.Map String Term -> [Term]-> (Bool,[Term],Map.Map String Term)

matchPredicate (Struct _ _ goalParams) restGoals oldUnification ((Struct _ _ ruleParams):conditions)  = 
	case matchParams goalParams ruleParams oldUnification of 
		(True,unification) -> (True, substitute  unification (conditions ++ restGoals) , unification)
		(False,_) -> (False, [], Map.empty)



combineSolution :: [(Bool,[Map.Map String Term],Int)] -> (Bool,[Map.Map String Term],Int)

combineSolution [] = (False,[],-1)
combineSolution (x : []) = x
combineSolution ((result, unification, seq) : xs) =
	let (r1, u1, seq1) = combineSolution xs
	in (result || r1, unification ++ u1, max seq seq1)

initRules :: [[Term]] -> Int -> ([[Term]],Int)

initRules [] seq = ([],seq)
initRules (x:xs) seq = 
	let (y,newSeq) = initRule x seq
	    (ys,finalSeq) = initRules xs newSeq
	in ((y:ys),finalSeq)

initRule :: [Term] -> Int ->([Term],Int)
initRule terms seq = 
	let (replacement,newSeq) = getReplacement (getVariables terms) seq Map.empty
	in (map (substituteTerm replacement) terms, newSeq)

getReplacement :: [String] -> Int ->Map.Map String Term-> (Map.Map String Term, Int)
getReplacement [] seq replacement = (replacement,seq)
getReplacement (x:xs) seq replacement = 
	getReplacement xs (seq+1) (Map.insert x (Variable ("TEMP"++(show seq))) replacement)


initTerm :: Term -> Int -> (Term,Int)
initTerm (Variable x) seq = (Variable ("TEMP"++(show seq)), seq+1)
initTerm (Struct name 0 []) seq = (Struct name 0 [],seq)
initTerm (Struct name paramNum params) seq =
	let (ps,newSeq) = initRule params seq
	in ((Struct name paramNum ps),newSeq)


matchStructs :: Term -> Term -> Map.Map String Term -> (Bool, Map.Map String Term)

matchStructs (Struct aName aNum aParams) (Struct bName bNum bParams) oldUnification
	| aName == bName && aNum == bNum =  matchParams aParams bParams oldUnification
	| otherwise = (False,Map.empty)

matchParams :: [Term] -> [Term] -> Map.Map String Term -> (Bool,Map.Map String Term)

matchParams [] [] oldUnification = (True,oldUnification)
matchParams (x:xs) (Variable(y):ys) oldUnification = 
	matchParams (substitute sub xs) (substitute sub ys) (Map.insert y x (substituteUnification sub oldUnification))
	where sub = Map.singleton y x
matchParams (Variable(x):xs) (y:ys) oldUnification = 
	matchParams  (substitute sub xs) (substitute sub ys) (Map.insert x y (substituteUnification sub oldUnification))
	where sub = Map.singleton x y
matchParams (x:xs) (y:ys) oldUnification = case matchStructs x y oldUnification of
	(True,unificaton) -> matchParams (substitute unificaton xs) (substitute unificaton ys) unificaton
	(False,_)		  -> (False,Map.empty)

substitute :: Map.Map String Term -> [Term] -> [Term]
substitute unification terms = map (substituteTerm unification) terms

substituteUnification :: Map.Map String Term -> Map.Map String Term -> Map.Map String Term
substituteUnification sub unification = Map.map (substituteTerm sub) unification

substituteTerm :: Map.Map String Term -> Term -> Term
substituteTerm unification (Variable x) = 
	case value of
		Nothing -> Variable x
		Just t  -> t
	where value = Map.lookup x unification

substituteTerm unification (Struct name 0 []) = Struct name 0 []
substituteTerm unification (Struct name paramNum params) = Struct name paramNum (map (substituteTerm unification) params)

--main = do
--	args <- getArgs
--	p <- parseFromFile prologFile (head args)
--	g <- parseFromFile prologFile (head (tail args))
--	case (p,g) of
--		(Right pl,Right gl) -> print $ solve pl (head gl) 
--		(Left pe, _)        -> print pe
--		(_, Left ge)        -> print ge





