-------------------------------------------------------------------------------
-- |
-- Module      :  Fluorine.Attributes
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- This module defines the `Event` monad.
------------------------------------------------------------------------------

module Fluorine.HTML.Events.Monad
  ( Event(..)
  -- , unEvent
  --
  -- , runEvent
  --
  -- , yield
  -- , async
  -- , andThen
  ) where

  -- | The `Event` monad, which supports the asynchronous generation of events.
-- |
-- | This monad is used in the definition of `runUI`.
newtype Event a = Event {unEvent :: [(IO a)]}

-- | Run a computation in the `Event` monad by providing a callback function.
--
--   The callback function will be invoked zero or more times.
-- XXX Todos
-- runEvent :: (Error -> IO ()) -> (a -> IO ()) -> Event a -> IO ()
-- runEvent f s = go <<< unEvent
--   where
--   go l = runAff f handler (later (uncons l))
--
--   handler :: Maybe (Tuple a (ListT (Aff eff) a)) -> Eff eff Unit
--   handler Nothing = return unit
--   handler (Just (Tuple a l)) = s a *> go l
