---- CS410 [Adv. Functional Programming] - Project #1, Dan Jang
---- Objective: Implement a simple dictionary compression algorithm that takes an input text-file and displays an compressed output.

--- Libraries
import System.IO
import System.Environment(getArgs)
import Data.List.Split(wordsBy) -- Note: Might require installation
import Data.Maybe(fromJust)
import Data.List(sortBy, groupBy, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- (Webster's) Dictionary Type
type Webster = Map.Map String Int
dictionarincation :: [String] -> Webster
dictionarincation words = Map.fromList $ zip (Set.toList $ Set.fromList words) [0..]

-- Input & Output
input :: Webster -> [String] -> [Int]
input dictionary = map(\word -> fromJust $ Map.lookup word dictionary)
output :: Webster -> [Int] -> [String]
output dictionary = map(\num -> fromJust $ Map.lookup num (Map.fromList $ Map.toList dictionary))
    where
        swap (x, y) = (y, x)

-- Text Conversion to Words
ttW :: String -> [String]
ttW = wordsBy(`elem` [' ','\t','\n'])

-- Main
main :: IO()
main = do
    argc <- getArgs
    case argc of
        [txtFile] -> do
            txt <- readTxt txtFile
            
            let ttWs = ttW txt
            let dictionary = dictionarincation ttWs
            let theinput = input dictionary ttWs
            let theoutput = output dictionary theinput

            putStrLn $ "The original text was: " ++ txt
            putStrLn $ "Here is the compressed text: " ++ (show theinput)
            putStrLn $ "Here is the decompressed text: " ++ (unwords theoutput)
            
        _ -> putStrLn $ "Command-line usage: runhaskell proj1.hs [text file path here!]"