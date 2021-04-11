module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = case xs of
    [] -> []
    (x:ts) -> if not (p x)
              then x : discard p ts
              else discard p ts

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = case xs of
    [] -> []
    (x:ts) -> if p x
              then x : keep p ts
              else keep p ts
