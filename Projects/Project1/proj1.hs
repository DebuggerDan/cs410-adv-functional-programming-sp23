---- CS410 [Adv. Functional Programming] - Project #1, Dan Jang
---- Objective: Implement a simple dictionary compression algorithm that takes an input text-file and displays an compressed output.

--- Libraries
import System.IO
import System.Environment(getArgs)
--import Data.List.Split(wordsBy) -- Note: Might require installation
import Data.List.Split(split, oneOf)--whenElt) -- Note: Might require installation
import Data.Maybe(fromJust)
import Data.List(sortBy, groupBy, sort)
import Data.Either(rights)
import Data.Char(isSpace)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- (Webster's) Dictionary Type
type Webster = Map.Map String Int

-- (Prev) Simple Dictionarin(i)cation
-- dictionarincation :: [String] -> Webster
-- dictionarincation words = Map.fromList $ zip (Set.toList $ Set.fromList words) [0..]

-- Dictionarinication v2
dictionarinication :: [Either String String] -> Webster
dictionarinication text = Map.fromList $ zip wordDefinitions [0..]

    where

        wordDefinitions = Map.keys . Map.fromList . map (\w -> (w, ())) . rights $ text--map snd . Map.toAscList . Map.fromList . map (flip (,) ()) . rights $ text
        --sentences = foldr (\a acc -> case a of Right word -> word:acc;_ -> acc) []


-- (Prev) Simple Input & Output
-- input :: Webster -> [String] -> [Int]
-- input dictionary = map(\word -> fromJust $ Map.lookup word dictionary)
-- output :: Webster -> [Int] -> [String]
-- output dictionary = map(\num -> fromJust $ Map.lookup num (Map.fromList $ map swap $ Map.toList dictionary))
--     where
--         swap (x, y) = (y, x)

-- Input & Output v2
input :: Webster -> [Either String String] -> [Either String Int]
input dictionary = map inputText

    where

        inputText inputtedtext = case inputtedtext of
            Right word -> Right $ fromJust $ Map.lookup word dictionary
            Left words -> Left words

output :: Webster -> [Either String Int] -> [Either String String]
output dictionary = map outputText--map(\num -> fromJust $ Map.lookup num (Map.fromList $ map swap $ Map.toList dictionary))
    
    where

        outputText outputtedtext = case outputtedtext of
            Right idx -> Right $ fromJust $ Map.lookup idx (Map.fromList $ map swap $ Map.toList dictionary)
            Left words -> Left words
        swap (x, y) = (y, x)


-- Load Pre-Compressed File for Decompression
-- load :: FilePath -> IO(Webster, [Either String Int])
-- load txtFile = do
--     text <- readFile txtFile
--     let (dictionaryLine:inputLine:_) = lines text
--     let dictionary = dictionarinication (ttW dictionaryLine)
--     let input = map(either Left(Right . read)) (ttW inputLine)
--     return (dictionary, input)

-- Load Pre-Compressed File for Decompression v2
load :: FilePath -> IO(Webster, [Either String Int])
load txtFile = do
    text <- readFile txtFile
    let dictionaryLine : inputLine = lines text
    let dictionary = dictionarinication (ttW dictionaryLine)
    let input = map(either Left(Right . read)) (ttW (unlines inputLine))
    return (dictionary, input)

-- Text Conversion to Words
ttW :: String -> [Either String String]-- -> Webster
--ttW "" = []-- = split (oneOf " \t\n") . lines--split (whenElt (`elem` [' ','\t','\n'])) . lines--wordsBy(`elem` [' ','\t','\n'])
ttW = concatMap ttWLine . lines

    where

        ttWLine "" = [Left "\n"]
        ttWLine e@(s:_)

            | s == '\n' = Left "\n" : ttWLine (tail e)
            | isSpace s = let (w, ord) = span isSpace e in Left w : ttWLine ord
            | otherwise = let (w, ord) = span (not . isSpace) e in Right w : ttWLine ord


-- Main
main :: IO()
main = do
    argc <- getArgs

    case argc of

        [option, txtFile] -> do

            txt <- readFile txtFile
            
            let words = ttW txt
            let dictionary = dictionarinication words
            --let theinput = input dictionary words
            --let theoutput = output dictionary theinput

            case option of

                "compress" -> do
                    let theinput = input dictionary words
                    putStr $ unwords (Map.keys dictionary)--"The original text was: " ++ --txt
                    putStrLn ""
                    mapM_ (either putStr (putStr . show)) theinput--"Here is the compressed text: " ++ (show theinput)
                
                "decompress" -> do
                    --putStrLn $ "Here is the decompressed text: " ++ (unwords theoutput)
                    (loadDictionary, loadInput) <- load txtFile
                    let decompressed = output loadDictionary loadInput
                    putStrLn ""
                    mapM_ (either putStr putStr) decompressed
                
                -- "both" -> do
                --     let theinput = input dictionary words
                --     let theoutput = output dictionary theinput
                --     putStrLn ""
                --     mapM_ (either putStr (putStr . show)) theinput
                --     putStrLn ""
                --     mapM_ (either putStr putStr) theoutput
                
                _ -> do
                    let theinput = input dictionary words
                    let theoutput = output dictionary theinput
                    putStrLn "Default option (both + original text) selected.\n"
                    putStr $ unwords (Map.keys dictionary)--"The original text was: " ++ --txt
                    putStrLn ""
                    mapM_ (either putStr (putStr . show)) theinput
                    putStrLn ""
                    mapM_ (either putStr putStr) theoutput
        
        _ -> putStrLn $ "Command-line usage: runhaskell proj1.hs [options: 'compress' or 'decompress'] [text file path here!]\nFor the option argument, you may also enter any other string besides above, except whitespaces, to have the program automatically both compress & decompress (assuming input text file starts off un-compressed)!"