{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Fluorine.Attributes
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- This module defines the `EventHandler` functor, which can be used
-- to perform standard operations on HTML events.
----------------------------------------------------------------------------

module Fluorine.HTML.Events.Handler
  ( EventHandler

  , preventDefault
  , stopPropagation
  , stopImmediatePropagation

  ) where

import Control.Monad.Writer

data EventUpdate
  = PreventDefault
  | StopPropagation
  | StopImmediatePropagation

-- | This monad supports the following operations on events:
--
--   - `preventDefault`
--   - `stopPropagation`
--   - `stopImmediatePropagation`
--
--   It can be used as follows:
--
--   import Control.Functor (($>))
--
--   H.a (E.onclick \_ -> E.preventDefault $> ClickHandler) (H.text "Click here")
newtype EventHandler a = EventHandler (Writer [EventUpdate] a)
  deriving (Functor, Applicative, Monad)

unEventHandler :: EventHandler a -> Writer [EventUpdate] a
unEventHandler (EventHandler mw) = mw

-- | Call the `preventDefault` method on the current event
preventDefault :: EventHandler ()
preventDefault = EventHandler (tell [PreventDefault])

-- | Call the `stopPropagation` method on the current event
stopPropagation :: EventHandler ()
stopPropagation = EventHandler (tell [StopPropagation])

-- | Call the `stopImmediatePropagation` method on the current event
stopImmediatePropagation :: EventHandler ()
stopImmediatePropagation = EventHandler (tell [StopImmediatePropagation])
