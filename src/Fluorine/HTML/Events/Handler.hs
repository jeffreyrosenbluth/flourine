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
  ( EventHandler()
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
