module PrologParser(
	Term(..),
	prologFile
--	consult
) where

import Text.ParserCombinators.Parsec
import System.Environment



data Term = 
      Variable String
	| Struct String Int [Term]

instance Show Term where
	show (Variable x) = x
	show (Struct name 0 []) = name
	show (Struct name _ (x:xs)) = name ++"(" ++ (foldl (\s t->s ++ ","++ (show t)) (show x) xs) ++")"


prologFile :: GenParser Char st [[Term]]
-- A prolog file contains 0 or more lines and is terminated by the end of the file
prologFile = do
	program <- sepBy line (char '\n')
	eof
	return program

line :: GenParser Char st [Term]
-- A line is either a fact or a rule( p1 :- p2 [,p3].)
line = do
	terms <- try fact
	     <|> try rule
	     <|> emptyLine

	return terms

emptyLine :: GenParser Char st [Term]

emptyLine = do 
	spaces
	return []

fact :: GenParser Char st [Term]
-- (with only one predicate followed by ".") 
fact = do
	s <- struct
	char '.'
	return [s]

rule :: GenParser Char st [Term]

rule = do
	s <- struct
	string ":-"
	ss <- sepBy struct (char ',')
	char '.'
	return (s:ss)

variable :: GenParser Char st Term

variable = do 
	c <- try upper
	 <|> char '_'
	cs <- many alphaNum
	return (Variable (c:cs))

struct :: GenParser Char st Term

struct = do
	spaces
	c <- lower
	 <|> digit
	cs <- many alphaNum
	p<- optionMaybe params
	spaces
	return (case p of
		Nothing -> Struct (c:cs) 0 []
		Just ps -> Struct (c:cs) (length ps) ps
		)

params :: GenParser Char st [Term]

params = do
	char '('
	ts <- sepBy term (char ',')
	char ')'
	return ts

term :: GenParser Char st Term

term = do
	t <- try variable
	 <|> struct
	return t

--consult :: String -> IO (ParseError String [[Term]])

--consult fileName = do
--	p <- parseFromFile prologFile fileName
--	p

main = do
	args <- getArgs
	p <- parseFromFile prologFile (head args)
	case p of 
		Left err -> print err
		Right pl -> print pl



