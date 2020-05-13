module Main where

import Control.Monad (forever)
import Data.Char (toLower, ord)
import Data.Maybe (isJust)
import Data.List (intersperse, nub)
import System.Exit(exitSuccess)
import System.Random (randomRIO)
import System.IO

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

isAlphaWord :: String -> Bool
isAlphaWord word = all isAlphaChar word

isAlphaChar :: Char -> Bool
isAlphaChar char = (ordChar >= ord 'a' && ordChar <= ord 'z') || (ordChar >= ord 'A' && ordChar <= ord 'Z')
  where ordChar = ord char

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)

gameLength :: String -> Bool
gameLength w
  = let l = length (w :: String)
    in      l >= minWordLength
         && l < maxWordLength

allGameWords :: IO WordList
allGameWords = do
  agw <- gameWords
  return (filter isAlphaWord agw)

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0,((-) (length wl) 1))
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = allGameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (foundWord word) guessedLetters
  where guessedLetters = [] :: [Char]
        foundWord word = map (const Nothing) word

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) letter = elem letter word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guesses) letter = elem letter guesses

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar letter =
  case letter of
    Nothing -> '_'
    Just char -> char

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character was not in the word, try again."
      return (fillInCharacter puzzle guess)

gameLimit :: Int
gameLimit = 7

numWrong :: String -> [Char] -> Int
numWrong word guesses = sum $ map (inWord word) guesses

inWord :: String -> Char -> Int
inWord word char
  | char `elem` word = 0
  | otherwise = 1

numWrong' :: Puzzle -> Int
numWrong' (Puzzle wordToGuess _ guessed) = numWrong wordToGuess guessed

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle wordToGuess _ guessed) =
  if (numWrong wordToGuess guessed) > gameLimit then
    do gameWin puzzle
       putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word filledInSoFar guesses) =
  if all isJust filledInSoFar then
    do putStrLn $ "Great job, you got the word " ++ word ++ " with " ++ (show $ gameLimit - (numWrong word guesses)) ++ " guesses left."
       putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStrLn $ "You have " ++ (show ((-) gameLimit (numWrong' puzzle))) ++ " incorrect guesses left!"
  putStrLn "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
