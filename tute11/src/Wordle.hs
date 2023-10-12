{-# LANGUAGE InstanceSigs #-}

module Wordle (module Wordle) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable
import Data.List
  ( delete,
    elemIndices,
    filter,
    lines,
    map,
    unwords,
    zip,
    zipWith,
    (!!),
    (++),
  )
import System.Console.ANSI
import Prelude

data WordleAnswers = Correct | Misplaced | Incorrect
  deriving (Eq)

instance Show WordleAnswers where
  show :: WordleAnswers -> String
  show Correct = "green"
  show Misplaced = "yellow"
  show Incorrect = "gray"

type InternalState = Int

-- | generate the next element in a pseudorandom sequence from the prevSeed
-- Taken from the Week 10 workshop!
nextRand :: InternalState -> InternalState
nextRand prevSeed = (a * prevSeed + c) `mod` m
  where
    -- Parameters for linear congruential RNG.
    a = 1664525
    c = 1013904223
    m = 2 ^ 32

-- | Read the "src/all_words.txt" file in to a list
-- This files contains all possible five letter words
-- wordle will accept
-- >>> length <$> validWords
-- 14855
validWords :: IO [String]
validWords = undefined

-- | Read the "src/possible_answers.txt" file in to a list
-- This files contains all possible answers wordle may use
-- >>> length <$> validAnswers
-- 2311
validAnswers :: IO [String]
validAnswers = undefined

-- | Generate a random Wordle answer
-- /Hint/: Use !! to index an array
-- >>> generateRandomAnswer 1 
-- (1015568748,"aback")
generateRandomAnswer :: InternalState -> IO (InternalState, String)
generateRandomAnswer = undefined

-- | Check if the user has guessed a valid word (a 5 letter word that is one of the validAnswers)
-- >>> checkValid "notValid"
-- False
-- >>> checkValid "hello"
-- True
-- >>> checkValid "waacs"
-- True
checkValid :: String -> IO Bool
checkValid = undefined

-- | Naive attempt to do wordle feedback
-- | Compare lists letter by letter
-- >>> makeFeedbackNaive "hello" "atoll"
-- [gray,gray,yellow,green,yellow]
-- >>> makeFeedbackNaive "clued" "queue"
-- [gray,gray,yellow,yellow,gray]
-- >>> makeFeedbackNaive "aaaaa" "abcde"
-- [green,yellow,yellow,yellow,yellow]
-- >>> makeFeedbackNaive "atlol" "goooy"
-- [gray,gray,gray,green,gray]
-- >>> makeFeedbackNaive "aaabb" "acada"
-- [green,yellow,green,gray,gray]
makeFeedbackNaive :: String -> String -> [WordleAnswers]
makeFeedbackNaive guess target = undefined

-- You now need to be doing the full wordle problem.
-- 
-- As a suggested solution:
-- We will be doing a better solution, which does two passes.
--
-- 1. First Pass: Identifying Correct and Incorrect Letters
-- To determine whether each letter in the guessed word is in the correct position.
-- Steps:
-- Compares each pair of characters between the guessed and target words.
-- If the characters are the same, it's marked as Correct.
-- If not, it's marked as Incorrect.
-- Generates a list of triples containing each pair of characters and their corresponding feedback.
--
-- 2. Second Pass: Adjusting Feedback for Misplaced Letters
-- Iterates over the feedback generated in the first pass (triples).
-- You will need to keep track of the unmatched characters and the current feedback
-- Updates the state each iteration:
-- If a letter is marked as Incorrect and is in the unmatched list, it's marked as Misplaced, and the letter is removed from the unmatched list.
-- If the feedback is not Incorrect, it is left unchanged.
-- Generates a list of updated feedback, considering misplaced letters.
--
-- >>> makeFeedback "hello" "atoll"
-- [gray,gray,yellow,green,yellow]
-- >>> makeFeedback "clued" "queue"
-- [gray,gray,yellow,yellow,gray]
-- >>> makeFeedback "aaaaa" "abcde"
-- [green,gray,gray,gray,gray]
-- >>> makeFeedback "atlol" "goooy"
-- [gray,gray,gray,green,gray]
-- >>> makeFeedback "aaabb" "acada"
-- [green,yellow,green,gray,gray]
makeFeedback :: String -> String -> [WordleAnswers]
makeFeedback = undefined

-- /* Wordle Hard Mode */
-- In hard mode all correct letters in a guess (yellow and green letters) must be used in the subsequent guess
-- for the game to detect the word as a legitimate guess.
-- 
-- | All Correct Letters must be in consecutive guesses
-- Order of arguments must be: previous guess, guess, target
-- >>> ensureGreens "slate" "slime" "slope"
-- True
-- >>> ensureGreens "slate" "plime" "slope"
-- False
ensureGreens :: String -> String -> String -> Bool
ensureGreens = undefined

count :: Eq a => a -> [a] -> Int
count a = length . elemIndices a

-- | Ensure that yellow letters are in the next guess (can be anywhere)
-- Order of arguments must be: previous guess, guess, target
-- /Hint/ count might be useful for this.
-- >>> ensureYellows "slate" "slime" "slope"
-- True
-- >>> ensureYellows "world" "dlrow" "orwld"
-- True
-- >>> ensureYellows "aorld" "xxxxx" "orwld"
-- False
ensureYellows :: String -> String -> String -> Bool
ensureYellows = undefined

-- | Make sure the guesses follows both ensureGreens and ensureYellows as well, as the guess not being the same as previous
-- Order of arguments must be: previous guess, guess, target
-- >>> ensureCriteria Nothing "stove" "ready"
-- True
-- >>> ensureCriteria (Just "aorld") "xxxxx" "orwld"
-- False
-- >>> ensureCriteria (Just "slate") "slime" "slope"
-- True
-- >>> ensureCriteria (Just "slate") "plime" "slope"
-- False
-- >>> ensureCriteria (Just "slate") "slate" "slope"
-- False
ensureCriteria :: Maybe String -> String -> String -> Bool
ensureCriteria = undefined
