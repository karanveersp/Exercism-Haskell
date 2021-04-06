module Acronym (abbreviate) where

import qualified Data.Text as T
import Data.Char (isUpper, isLower, toUpper)

abbreviate :: String -> String
abbreviate xs =
    concatMap abbrevWord (T.words $ removeSpecialChars $ T.pack xs)
    where abbrevWord word = if all isUpper str || all isLower str
                            then [toUpper (head str)]
                            else filter isUpper str
                            where str = T.unpack word
          removeSpecialChars t = T.replace hiphen space $ T.replace underscore space t
                                 where hiphen = T.pack "-"
                                       underscore = T.pack "_"
                                       space = T.pack " "
