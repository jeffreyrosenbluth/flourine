-------------------------------------------------------------------------------
-- |
-- Module      :  Fluorine.Signal
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
-------------------------------------------------------------------------------

module Fluorine.Signal where

import Control.Arrow hiding (loop)
import Control.Category
import Data.Profunctor
import Prelude hiding (id, (.))

-- | A `SF` represents a state machine which responds to inputs of type `i`, producing outputs of type `o`.
newtype SF i o = SF {runSF :: (i -> SF1 i o)}

-- | `SF1` represents non-empty signals, i.e. signals with an initial output value.
newtype SF1 i o = SF1 {runSF1 :: (o, SF i o)}

-- | A `SF` which returns the latest input
input :: SF i i
input = SF $ \i -> SF1 (i, input)

-- | Convert a `SF` to a `SF1` by providing an initial value
startingAt :: SF i o -> o -> SF1 i o
startingAt s o = SF1 (o, s)

-- | Get the current value of a `SF1`.
result :: SF1 i o -> o
result (SF1 p) = fst p

-- | Convert a `SF1` to a `SF` by ignoring its initial value.
next :: SF1 i o -> SF i o
next (SF1 p) = snd p

-- | Creates a stateful `SF1`
stateful :: s -> (s -> i -> s) -> SF1 i s
stateful s step = stateful' s (\t i -> let s' = step t i in (s', s')) `startingAt` s

-- | Creates a stateful `SF` based on a function which returns an output value.
stateful' :: s -> (s -> i -> (o, s)) -> SF i o
stateful' s step = go s
  where
  go t = SF $ \i ->
    let (o, s') = step t i
    in  SF1 (o, go s')

-- | A `SF` which compares consecutive inputs using a helper function.
differencesWith :: (i -> i -> d) -> i -> SF i d
differencesWith f initial = stateful' initial $ \l n ->
  let d = f l n
  in (d, n)

-- | Create a `SF` which hides a piece of internal state of type `s`.
--   XXX Should 'SF' be an instance of 'ArrowLoop' ?
loop :: s -> SF (s, i) (s, o) -> SF i o
loop s signal = SF $ \i ->
  let (SF1 p) = runSF signal (s, i)
  in  SF1 (snd $ fst p, loop (fst $ fst p) (snd p))

-- | Merge two non-empty signals, outputting the latest value from both
-- | signals at each step.
mergeWith :: (c -> d -> r) -> SF1 a c -> SF1 b d -> SF1 (Either a b) r
mergeWith = mergeWith' id

-- | A variant of `mergeWith` which takes an additional function to destructure
-- | its inputs.
mergeWith' ::  (i -> Either a b) -> (c -> d -> r) -> SF1 a c -> SF1 b d -> SF1 i r
mergeWith' f g = o
  where
  o s1 s2 = SF1
    (g (result s1) (result s2), SF $ \i ->
        case f i of
          Left a  -> runSF (next s1) a `o` s2
          Right b -> s1 `o` runSF (next s2) b
    )

instance Functor (SF i) where
  fmap f (SF k) = SF $ \i -> f <$> k i

instance Functor (SF1 i) where
  fmap f o = SF1 (f $ result o, f <$> next o)

instance Applicative (SF i) where
  pure a = SF $ \_ -> pure a
  (<*>) f x = SF $ \i -> runSF f i <*> runSF x i

instance Applicative (SF1 i) where
  pure a = SF1 (a, pure a)
  (<*>) f x = SF1 (result f $ result x, next f <*> next x)

instance Profunctor SF where
  dimap f g (SF k) = SF $ \i -> dimap f g (k (f i))

instance Profunctor SF1 where
  dimap f g o = SF1 (g $ result o, dimap f g (next o))

instance Category SF where
  id = input
  f . g = SF $ \i -> let s1 = runSF g i
                         s2 = runSF f (result s1)
                     in  SF1 (result s2, next s2 . next s1)

instance Arrow SF where
  arr f   = SF $ \i ->  SF1 (f i, arr f )
  first s = SF $ \(a, c) -> let o = runSF s a
                            in  SF1 ((result o, c), first $ next o)

instance ArrowChoice SF where
  left s = SF $ \e ->
      case e of
        Left a  -> let o = runSF s a
                   in  SF1 (Left $ result o, left $ next o)
        Right c -> SF1 (Right c, left s)
