-------------------------------------------------------------------------------
-- |
-- Module      :  Fluorine.Attributes
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Convenience functions for working with form elements.
------------------------------------------------------------------------------

module Fluorine.HTML.Events.Forms
  ( onValueChanged
  , onChecked
  , onInput
  ) where

import           Control.Applicative

import           Fluorine.HTML.Events.Handler
import qualified Fluorine.HTML.Attributes as H

-- | Attach event handler to event ```key``` with getting ```prop``` field
--   as an argument of handler
addForeignPropHandler :: (Alternative f) -- , IsForeign value)
                      => String -> String -> (value -> EventHandler (f i)) -> H.Attr (f i)
addForeignPropHandler = undefined
-- addForeignPropHandler key prop f =  H.handler (H.eventName key) (\e -> handler (toForeign e.target))
  -- where
  -- handler :: Foreign -> EventHandler (f i)
  -- handler e = case readProp prop e of
  --               Left _ -> pure empty
  --               Right i -> f i

-- | Attach an event handler which will produce an input when the value of an input field changes
--
--   An input will not be produced if the value cannot be cast to the appropriate type.
onValueChanged :: (Alternative f) {- , IsForeign value) -} => (value -> EventHandler (f i)) -> H.Attr (f i)
onValueChanged = addForeignPropHandler "change" "value"

-- | Attach an event handler which will fire when a checkbox is checked or unchecked
onChecked :: Alternative f => (Bool -> EventHandler (f i)) -> H.Attr (f i)
onChecked = addForeignPropHandler "change" "checked"

-- | Attach an event handler which will fire on input
onInput :: Alternative f => (String -> EventHandler (f i)) -> H.Attr (f i)
onInput = addForeignPropHandler "input" "value"
