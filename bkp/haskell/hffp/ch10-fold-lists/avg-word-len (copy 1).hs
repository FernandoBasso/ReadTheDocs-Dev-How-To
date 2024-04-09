{-# LANGUAGE NoMonomorphismRestriction #-}

f :: [Char] -> Int
f x = div (sum (map length (words x)))
          (length (words x))

g :: [Char] -> Double
g x = (/) (fromIntegral (sum (map length (words x))))
          (fromIntegral (length (words x)))

--
-- λ> g "may the force"
-- 3.6666666666666665
-- λ> f "may the force"
-- 3
--
-- The name of this function could be 'wordLengthAverage' or similar, because
-- it produces the average of the length of words in a sentence. The param 'x'
-- would also be better named as 's' since programmers have a tradition of
-- using things like 's' for strings, 'n' or 'i' for a number, 'c' for a
-- character, among some other informal standards.
--

avgWordLen :: String -> Double
avgWordLen s = (/) numChars numWords
  where
    numChars :: Double
    numChars = fromIntegral $ sum $ map length $ words s
    numWords :: Double
    numWords = fromIntegral $ length $ words s
--
-- λ> avgWordLen "The force is strong with this one. Most impressive!"
-- 4.777777777777778
--
