import PrologParser
import PrologSolver
import Text.ParserCombinators.Parsec
import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as Map


data Command = 
	  Load String
	| Append String
	| Query String
	| Quit
	deriving (Show)
	
	
parseCommand :: String -> Command

parseCommand (':':'l':' ':path) = Load path
parseCommand (':':'a':' ':path) = Append path
parseCommand (':':'q':_) = Quit
parseCommand query = Query query

execute :: [[Term]] -> IO ()

execute program = do
	c <- getLine
	case parseCommand c of
		Load path -> do
			print $ "loaded from " ++ path
			p <- parseFromFile prologFile path
			case p of
				Right pl ->
				    execute pl
				Left pe -> do
					print pe
					execute program
		Append path -> do
			p <- parseFromFile prologFile path
			case p of
				Right pl -> execute $ pl++program
				Left pe -> do
					print pe
					execute program
		Quit -> return ()
		Query query -> do
			g <-return $ parse prologFile "(unknown)" query
			case g of
				Right gl -> do
					printResult $ solve program (head gl)
					execute program
				Left ge -> do 
					print ge
					execute program

printResult :: (Bool,[Map.Map String Term]) -> IO ()

printResult (result, unification) = do
	print result
	mapM_ (mapM_ print . Map.toList) unification



main = do
	execute []
	

    