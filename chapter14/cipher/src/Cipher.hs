module Cipher
  ( Cipher
  , caeser
  , uncaeser
  , vigenere
  , unvigenere )
where

import Data.Char

type Cipher = String
type Message = String

caeser :: Int -> Message -> Cipher
caeser rshift string = map (encryptLetter ((+) rshift)) string


encryptLetter :: (Int -> Int) -> Char -> Char
encryptLetter rshiftOp letter
  | elem letter allLetters = changeLetter rshiftOp letter
  | otherwise = letter
  where allLetters = ['a'..'z'] ++ ['A'..'Z']

changeLetter :: (Int -> Int) -> Char -> Char
changeLetter shiftOp letter = chr $ encodeLetter shiftOp letter

encodeLetter :: (Int -> Int) -> Char -> Int
encodeLetter shiftOp letter
  | elem letter ['a'..'z'] = mod (shiftOp (ord letter - ord 'a')) 26 + ord 'a'
  | elem letter ['A'..'Z'] = mod (shiftOp (ord letter - ord 'A')) 26 + ord 'A'
  | otherwise = ord letter

uncaeser :: Int -> Cipher -> Message
uncaeser rshift string = map (encryptLetter ((flip (-)) rshift)) string

type Key = String

shift :: (Int -> Int -> Int) -> (Char, Char) -> Char
shift operation (charOffset, letter) = changeLetter (operation (intOffset charOffset)) letter where
  intOffset char
    | elem char ['a'..'z'] = ord char - ord 'a'
    | elem char ['A'..'Z'] = ord char - ord 'A'
    | otherwise = 0

createShiftString :: Key -> String -> Int -> String
createShiftString _ [] _ = []
createShiftString keyword (m:message) keyloc
  | elem m ['a'..'z'] = [(keyword !! keyloc)] ++ createShiftString keyword message (newKeyLoc keyloc)
  | elem m ['A'..'Z'] = [(keyword !! keyloc)] ++ createShiftString keyword message (newKeyLoc keyloc)
  | otherwise = [m] ++ createShiftString keyword message keyloc
  where newKeyLoc location = mod (location + 1) (length keyword)

vigenere :: Key -> Message -> Cipher
vigenere keyword message = fmap (shift (+)) (zip (createShiftString keyword message 0) message)

unvigenere :: Key -> Cipher -> Message
unvigenere keyword cipher = fmap (shift (flip (-))) (zip (createShiftString keyword cipher 0) cipher)
