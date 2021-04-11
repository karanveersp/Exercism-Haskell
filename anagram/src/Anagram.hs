module Anagram (anagramsFor) where
import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter isAnargram
    where isAnargram ys = lower xs /= lower ys && (sort (lower xs) == sort (lower ys))
                          where lower ts = map toLower ts


