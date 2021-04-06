module Acronym (abbreviate) where

import qualified Data.Text as T
import Data.Char (isUpper, isLower, toUpper)

abbreviate :: String -> String
abbreviate xs = 
    concat $ map getUppers $ T.words $ removeSpecialChars $ T.pack xs
    where getUppers x = do if all isUpper str || all isLower str
                           then [toUpper $ head str]
                           else filter isUpper str
                           where str = T.unpack x
          removeSpecialChars t = T.replace hiphen space $ T.replace underscore space t
                           where hiphen = T.pack "-"
                                 underscore = T.pack "_"
                                 space = T.pack " "