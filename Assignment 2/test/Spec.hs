import System.FilePath.Glob (glob)
import Control.Monad(mapM_, (>>=), (>>))
import Data.Function (flip)
import Assignment 
import Instances
import Parser
import Data.List (isPrefixOf, sort)
import Data.Maybe
import Data.Either (rights)
import Control.Exception (try, IOException)
import System.IO.Error (isDoesNotExistError)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (splitDirectories)

-- Function to replace a substring with another substring in a string
replaceSubstring :: String -> String -> String -> String
replaceSubstring _ _ [] = []
replaceSubstring from to str@(x:xs)
  | from `isPrefixOf` str = to ++ replaceSubstring from to (drop (length from) str)
  | otherwise = x : replaceSubstring from to xs

chooseParser :: Char -> (Parser ADT, ADT -> String)
chooseParser 'A' = (parseExerciseA, prettyPrintExerciseA)
chooseParser 'B' = (parseExerciseB, prettyPrintExerciseB)
chooseParser 'C' = (parseExerciseC, prettyPrintExerciseC)
chooseParser _ = undefined

-- https://stackoverflow.com/questions/4503958/what-is-the-best-way-to-split-a-string-by-a-delimiter-functionally
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delimiter = foldr f [[]]
  where
    f c l@(x:xs)
      | c == delimiter = [] : l      -- If the character matches the delimiter, start a new sublist.
      | otherwise = (c : x) : xs     -- If not, add the character to the current sublist.


extractLetter :: FilePath -> Char
extractLetter filePath =
  case splitDirectories filePath of
    ("javascript" : "inputs" : letter : _) -> head letter
    _ -> error "Invalid file path format"


processFile :: FilePath -> IO ()
processFile f = do
  result <- try (readFile f) :: IO (Either IOException String)
  case result of
    Left e
      | isDoesNotExistError e -> putStrLn $ "File not found: " ++ f
      | otherwise -> putStrLn $ "Error reading file " ++ f ++ ": " ++ show e
    Right contents -> do
      let letter = extractLetter f
      let (parser, prettyPrinter) = chooseParser letter
      let parsed = parse parser contents
      case parsed of
        Result _ adt -> do
          let prettyOutput = prettyPrinter adt
          let output_file = replaceSubstring "inputs" "output" f
          let outputDir = "javascript/output/" ++ [letter]
          createDirectoryIfMissing True outputDir  -- Create the directory if it doesn't exist

          writeFile output_file prettyOutput
          putStrLn $ "Processed: " ++ f
        _ -> putStrLn $ "Invalid ParseResult for file " ++ f

testTail :: FilePath -> IO (Maybe Bool)
testTail f = do
  result <- try (readFile f) :: IO (Either IOException String)
  case result of
    Left e
      | isDoesNotExistError e -> do
          putStrLn $ "File not found: " ++ f
          return Nothing
      | otherwise ->
          do
            putStrLn $ "Error reading file " ++ f ++ ": " ++ show e
            return Nothing
    Right contents -> do
        let bool = isTailRecursive contents
        return (Just bool)

          

main :: IO ()
main = do
  putStrLn ""
  files <- glob "javascript/inputs/*/*.js"
  mapM_ processFile files

  trChecks <- sort <$> glob "javascript/inputs/C/tailRecursive*.js"
  r <- (maybe "" show <$>) <$> mapM testTail trChecks
  let output = "const tailRecursiveResults = " ++ show r ++ ";"
  let output_file = "javascript/output/tailRecursiveOutput.js"
  writeFile output_file output