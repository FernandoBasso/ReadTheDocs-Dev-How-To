module Ch5 where

import Prelude (Unit, show, discard)

--
-- $ npx spago install lists
--
import Data.List (List(Cons, Nil), (:))

import Effect (Effect)
import Effect.Console (log)

flip :: ∀ (a :: Type) (b :: Type) (c :: Type). (a -> b -> c) -> b -> a -> c
flip f x y = f y x

const :: ∀ a b. a -> b -> a
const x _ = x

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

--
-- Zero is the lowest precedence.
--
infixr 0 apply as $

--
-- The type signature is the flipped version of the type signature
-- of ‘apply’, which means we can writ ‘applyFlipped’ in terms of
-- ‘apply’.
--
applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply

--
-- Or the “manually written” (as if the above is not manually
-- written 🤣) version:
--
applyFlipped' :: ∀ a b. a -> (a -> b) -> b
applyFlipped' x f = f x

--
-- Make it 1 not to conflict with ‘$’ when both ‘#’ and
-- ‘$’ are used together.
--
infixl 1 applyFlipped as #

singleton :: ∀ a. a -> List a
singleton e = Cons e Nil
--
-- Using `:` istead of `Cons`:
--
--   singleton e = e : Nil
--

test :: Effect Unit
test = do
  --
  -- log (show (flip const 1 2))
  --
  log $ show $ flip const 1 2
  flip const 1 2 # show # log

  log $ show $ singleton "xyz"
