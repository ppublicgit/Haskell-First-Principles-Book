module Main where

import Cipher
import Test.QuickCheck
import Data.Char (chr)

type Message = String
type Key = String

genChar :: Gen Char
genChar = elements $ map chr ascii_ints
  where ascii_ints = [0..127]

genLetter :: Gen Char
genLetter = elements $ ['a'..'z'] ++ ['A'..'Z']

genMessage :: Gen Message
genMessage = listOf genChar

genKey :: Gen Key
genKey = listOf genLetter

prop_caeserIsomorphism :: Property
prop_caeserIsomorphism =
  forAll (arbitrary :: Gen Int) $ \ shift ->
  forAll (genMessage) $ \ phrase ->
  (caeserIdentity shift) phrase == phrase

prop_vigenereIsomorphism :: Property
prop_vigenereIsomorphism =
  forAll (genMessage `suchThat` (not . null)) $ \ key ->
  forAll (genKey) $ \ phrase ->
  vigenereIdentity key phrase == phrase

caeserIdentity :: Int -> Message -> Message
caeserIdentity shift text = uncaeser shift $ caeser shift text

vigenereIdentity :: Key -> Message -> Message
vigenereIdentity key text = unvigenere key $ vigenere key text

--return []
main :: IO ()
main = do
  quickCheck prop_vigenereIsomorphism
  quickCheck prop_caeserIsomorphism
  return ()
