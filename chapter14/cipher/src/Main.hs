module Main where

import Cipher (caeser, uncaeser, vigenere, unvigenere)
import System.IO

caeserEncrypt :: String -> String -> IO ()
caeserEncrypt inputFile outputFile = do
  contents <- readFile inputFile
  let linesList = lines contents
  putStr $ "What is the caeser integer key?"
  keyInput <- getLine
  let keyInt = (read keyInput :: Int)
  let encryptedContents = unlines $ map (caeser keyInt) linesList
  writeFile outputFile encryptedContents
  putStrLn $ "Encrypted file " ++ inputFile ++ " to " ++ outputFile ++ " using a caeser cipher."
  return ()

caeserDecrypt :: String -> String -> IO ()
caeserDecrypt inputFile outputFile = do
  contents <- readFile inputFile
  let linesList = lines contents
  putStr $ "What is the caeser integer key?"
  keyInput <- getLine
  let keyInt = (read keyInput :: Int)
  let encryptedContents = unlines $ map (uncaeser keyInt) linesList
  writeFile outputFile encryptedContents
  putStrLn $ "Decrypted file " ++ inputFile ++ " to " ++ outputFile ++ " using a caeser cipher."
  return ()

vigenereEncrypt :: String -> String -> IO ()
vigenereEncrypt inputFile outputFile = do
  contents <- readFile inputFile
  let linesList = lines contents
  putStr $ "What is the vigenere keyword?"
  keyword <- getLine
  let encryptedContents = unlines $ map (vigenere keyword) linesList
  writeFile outputFile encryptedContents
  putStrLn $ "Encrypted file " ++ inputFile ++ " to " ++ outputFile ++ " using a vigenere cipher."
  return ()

vigenereDecrypt :: String -> String -> IO ()
vigenereDecrypt inputFile outputFile = do
  contents <- readFile inputFile
  let linesList = lines contents
  putStr $ "What is the vigenere keyword?"
  keyword <- getLine
  let encryptedContents = unlines $ map (unvigenere keyword) linesList
  writeFile outputFile encryptedContents
  putStrLn $ "Decrypted file " ++ inputFile ++ " to " ++ outputFile ++ " using a vigenere cipher."
  return ()

errorEncrypt :: IO ()
errorEncrypt = do
  putStrLn $ "Error: encryption/decryption choice not recognized. Please select caeser/decrypt caeser/vigenere/decrypt vigenere as one of (c/dc/v/dv)"
  return ()

encryptTextFile :: String -> String -> String -> IO ()
encryptTextFile encryptionType inputFile outputFile = do
  case encryptionType of
    "c" -> caeserEncrypt inputFile outputFile
    "v" -> vigenereEncrypt inputFile outputFile
    "dc" -> caeserDecrypt inputFile outputFile
    "dv" -> vigenereDecrypt inputFile outputFile
    _ -> errorEncrypt

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr $ "Would you like to encrypt/decrypt with caeser or vigenere? (c/dc/v/dv): "
  encryptionChoice <- getLine
  putStr $ "What is the input text file name? (specify full path): "
  filename <- getLine
  putStr $ "What is the output text file name? (specify full path): "
  newfilename <- getLine
  encryptTextFile encryptionChoice filename newfilename
  return ()
