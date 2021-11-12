module Frabble.Dictionary where

import Data.Char
import Frabble.Types
import Frabble.Useful

type Dictionary = [String]

getDictionary :: IO Dictionary
getDictionary = do
    dict <- readFile "Frabble/dictionary.txt"
    return (words dict)

checkDictionary :: Dictionary -> [String] -> [String]
checkDictionary _ [] = []
checkDictionary dict (w:ws)
    | map toLower w `elem` dict = checkDictionary dict ws
    | otherwise                 = w : checkDictionary dict ws
