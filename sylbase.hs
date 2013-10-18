module Sylbase where

consonants :: [Char]
consonants = "bcdfghjklmnprstvwxz"

vowels :: [Char]
vowels = "aeiouy"

toSyllables :: Int -> String
toSyllables n = concatMap toSyllable (blocks n)

blocks :: Int -> [Int]
blocks 0 = [0]
blocks n = blocks' n

blockSize = 2048

blocks' 0 = []
blocks' n = (n `mod` blockSize):(blocks' $ n `div` blockSize)

toSyllable :: Int -> String
toSyllable block = 
  let index1 = block `mod` (length consonants)
      index2 = block `div` (length consonants) `mod` (length vowels)
      index3 = block `div` ((length consonants) + (length vowels)) `mod` (length consonants)
  in (consonants !! index1) : (vowels !! index2) : (consonants !! index3) : []
