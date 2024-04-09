{-# LANGUAGE NoMonomorphismRestriction #-}

stops :: [Char]
stops = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

-- Produces all possible three-tuples of stop-vowel-stop combinations.
allCombs :: [Char] -> [Char] -> [(Char, Char, Char)]
allCombs ss vs = [(s, v, s) | s <- ss, v <- vs]

-- Checks whether a tuple starts with the given letter.
startsWith :: Char -> (Char, Char, Char) -> Bool
startsWith c (e, _, _) = c == e

startsWithAorP :: (Char, Char, Char) -> Bool
startsWithAorP (x, _, _) = x == 'a' || x == 'p'

nouns :: [[Char]]
nouns = ["jedi", "padawan", "kitten"]

verbs :: [[Char]]
verbs = ["fight", "run", "meow"]

mkNounVerbNoun :: [[Char]] -> [[Char]] -> [([Char], [Char], [Char])]
mkNounVerbNoun ns vs = [(n, v, n) | n <- ns, v <- vs]



