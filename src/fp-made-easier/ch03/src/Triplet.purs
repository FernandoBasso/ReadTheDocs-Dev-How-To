-- vim: set tw=68:

module Triplet where

{-

```
$ spago install strings
```

It will be added to `spago.dhall` as a dependency.


-}

import Prelude
import Data.String.CodeUnits (toCharArray)
import Data.Array (filter, length)
import Data.String as String

--
-- ‘a’, ‘b’ and ‘c’ are fully polymorphic. They can be any type
-- whatsoever.
--
data Triplet a b c = Triplet a b c
--
-- The Triplet Data Constructor has this implicit type:
--
--   Triplet :: a -> b -> c -> Triplet
--


triplet :: Triplet String Int Int
triplet = Triplet "NPC1" 3 7

zyzCoords :: Triplet Int Int Int
zyzCoords = Triplet 1 (-5) 7

--
-- Assume c is lowercase.
--
-- Not using sectioning like in Haskell. Not sure yet if it
-- is possible in PureScript.
--
isVowel :: Char -> Boolean
isVowel c = length (filter (\e -> e == c) ['a', 'e', 'i', 'o', 'u']) > 0

--
-- Assume s is lowercase.
--
countVowels :: String -> Int
countVowels s = length $ filter isVowel (toCharArray s)

type StringStats = Triplet String Int Int


getStats :: String -> StringStats
getStats s = Triplet s (String.length s) (countVowels s)

