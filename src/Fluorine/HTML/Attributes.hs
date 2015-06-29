{-# LANGUAGE ExistentialQuantification #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Fluorine.Attributes
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--

-- This module enumerates some common HTML attributes, and provides additional
-- helper functions for working with CSS classes.
-------------------------------------------------------------------------------

module Fluorine.HTML.Attributes
  ( AttrF(..)
  , HandlerF(..)
  , Attr(..)

  , AttributeName()
  , attributeName
  , runAttributeName

  , EventName()
  , eventName
  , runEventName
  ) where

import Fluorine.HTML.Events.Types
import Fluorine.HTML.Events.Handler

-- | The data which represents a typed attribute, hidden inside an existential package in
--   the `Attr` type.
data AttrF value = AttrF (AttributeName value -> value -> String) (AttributeName value) value

-- | The data which represents a typed event handler, hidden inside an existential package in
--   the `Attr` type.
data HandlerF i fields = HandlerF (EventName fields) (Event fields -> EventHandler i)

-- | A single attribute is either
--
--   - An attribute
--   - An event handler
data Attr i
  = forall v. Attr (AttrF v)
  | forall f. Handler (HandlerF i f)
  | Initializer i
  | Finalizer i

instance Functor Attr where
  fmap _ (Attr e) = Attr e
  fmap _ (Handler _) = undefined --XXX figure out what to do here. runExistsR (\(HandlerF name k) -> Handler (mkExistsR (HandlerF name (\e -> f <$> k e)))) e
  fmap f (Initializer i) = Initializer (f i)
  fmap f (Finalizer i) = Finalizer (f i)
-- | A type-safe wrapper for attribute names
--
--   The phantom type `value` describes the type of value which this attribute requires.
newtype AttributeName value = AttributeName String

-- | Create an attribute name
attributeName :: String -> AttributeName value
attributeName = AttributeName

-- | Unpack an attribute name
runAttributeName :: AttributeName value -> String
runAttributeName (AttributeName s) = s

-- | A type-safe wrapper for event names.
--
--   The phantom type `fields` describes the event type which we can expect to exist on events
--   corresponding to this name.
newtype EventName fields = EventName String

-- Create an event name
eventName :: String -> EventName fields
eventName = EventName

-- | Unpack an event name
runEventName :: EventName fields -> String
runEventName (EventName s) = s
