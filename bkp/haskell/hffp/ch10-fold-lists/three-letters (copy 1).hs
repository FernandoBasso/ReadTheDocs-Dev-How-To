{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- foldr :: (a -> b -> b) -> b -> [] a -> b
--
-- Or                                the list of elements
--                                    to be processed
--                                            |
--                                            |
--                                         -------
-- foldr :: (elem -> acc -> acc) -> acc -> [] elem -> acc
--          --------------------    ---               ---
--                 /                 |                 |
--                /                  |                 |
--         folding function   initial value for    the final result value
--                             the accumulator     accumulated during the
--                                                 entire process
--
-- That is, the folding function takes the current element of the list and the
-- accumulator. The accumulator is either the initial value for the
-- accumulator or the value accumulated so far as the list is being processed.
--
--
-- foldl :: (b -> a -> b) -> b -> [] a -> b
--
-- Or                                the list of elements
--                                    to be processed
--                                            |
--                                            |
--                                         -------
-- foldl :: (acc -> elem -> acc) -> acc -> [] elem -> acc
--          --------------------    ---               ---
--                 /                 |                 |
--                /                  |                 |
--         folding function   initial value for    the final result value
--                             the accumulator     accumulated during the
--                                                 entire process
--
-- BOHOLD! We are talking about foldl now. The folding function takes the
-- accumulator as its first argument and an element of the list as the second
-- argument.  foldr and foldl have a different order for their first and
-- second arguments. BEWARE!
--

--
-- Takes three letters of each string and concatenates them.
--

fr :: [Char]
fr = foldr (\str acc -> acc ++ take 3 str) "" ["Pizza", "Apple", "Banana"]

{-
foldr (\e _ -> take 3 e) "" ["Arch Linux", "OpenBSD", "Ubuntu"]
take 3 "Arch Linux"
"Arc"

WRONG: The folding function is passed "Pizza" first, takes 3, no evaluation of
the “rest of the fold” is forced, and immediatelly return "Arc".

The folding *does* terminate immediately after producing "Arc", but not
because of `take 3`. Rather, it is because we are not using the accumulator.


Example from @Morrow in haskell-beginners Discord room:

foldr (\e _ -> take 3 e) "" ["Arch Linux", "OpenBSD", "Ubuntu"]
-- is equivalent to
= "Arch Linux" `f` ("OpenBSD" `f` ("Ubuntu" `f` ""))
-- where `f` is the aforementioned lambda
-- So, we have something that looks like
= f "Arch Linux" blah
-- Since `f` ignores its second argument it won't even be evaluated.
= take 3 "Arch Linux"
= "Arc"
-}


fl :: [Char]
fl = foldl (\str acc -> acc ++ take 3 str) "" ["Pizza", "Apple", "Banana"]

{-
foldl :: (b -> a -> b) -> b -> [] a -> b
foldl _ z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
                            --!--

Note that the first argument for the `foldl` folding function is `z`, not an
element from the list. `f z e`, not `f e z`.

f = (\acc elem -> take 3 acc)
foldl f "" ["Arch Linux", "OpenBSD", "Ubuntu"]
      take 3 ""
      ""

foldl f "" ["OpenBSD", "Ubuntu"]
      take 3 ""
      ""

foldl f "" ["Ubuntu"]
      take 3 ""
      ""

foldl f "" []
      reaches base case, return acc which is "" in this case

fold
-}

