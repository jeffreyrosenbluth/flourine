{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}

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
( ClassName()
  , className
  , runClassName

  , AttributeName()
  , attributeName
  , runAttributeName

  , EventName()
  , eventName
  , runEventName

  , IsAttribute
  , toAttrString

  , AttrF(..)
  , HandlerF(..)

  , Attr(..)

  , attr
  , handler
  , initializer
  , finalizer

  , key

  , alt
  , charset
  , class_
  , classes
  , colSpan
  , rowSpan
  , content
  , for
  , height
  , href
  , httpEquiv
  , id_
  , name
  , rel
  , src
  , target
  , title
  , type_
  , value
  , width
  , disabled
  , required
  , readonly
  , spellcheck
  , enabled
  , checked
  , selected
  , placeholder
  ) where

import Data.List                    (intercalate)
import Fluorine.HTML.Events.Types   hiding (target)
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

-- | Create an attribute
attr :: IsAttribute value => AttributeName value -> value -> Attr i
attr n v = Attr (AttrF toAttrString n v)

-- | Create an event handler
handler :: EventName fields -> (Event fields -> EventHandler i) -> Attr i
handler n k = Handler (HandlerF n k)


-- | Attach an initializer.
initializer :: i -> Attr i
initializer = Initializer

-- | Attach a finalizer.
finalizer :: i -> Attr i
finalizer = Finalizer

-- | A wrapper for strings which are used as CSS classes
newtype ClassName = ClassName {runClassName :: String}

-- Create a class name
className :: String -> ClassName
className = ClassName

-- | A type-safe wrapper for attribute names
--
--   The phantom type `value` describes the type of value which this attribute requires.
newtype AttributeName value = AttributeName {runAttributeName :: String}

-- | Create an attribute name
attributeName :: String -> AttributeName value
attributeName = AttributeName

-- | A type-safe wrapper for event names.
--
--   The phantom type `fields` describes the event type which we can expect to exist on events
--   corresponding to this name.
newtype EventName fields = EventName {runEventName :: String}

-- Create an event name
eventName :: String -> EventName fields
eventName = EventName

-- | This type class captures those types which can be used as attribute values.
--
--   `toAttrString` is an alternative to `show`, and is needed by `attr` in the string renderer.
class IsAttribute a where
  toAttrString :: AttributeName a -> a -> String

instance IsAttribute [Char] where
  toAttrString _ s = s

instance IsAttribute Int where
  toAttrString _ n = show n

instance IsAttribute Bool where
  toAttrString n True = runAttributeName n
  toAttrString _ False = ""

-- | The `key` property associates a unique key with a node, which can be used to
--   implement a more efficient diff/patch.
key :: String -> Attr i
key = attr $ attributeName "key"

alt :: String -> Attr i
alt = attr $ attributeName "alt"

charset :: String -> Attr i
charset = attr $ attributeName "charset"

class_ :: ClassName -> Attr i
class_ = attr (attributeName "className") . runClassName

classes :: [ClassName] -> Attr i
classes ss = attr (attributeName "className") (intercalate " " $ map runClassName ss)

colSpan :: Int -> Attr i
colSpan = attr (attributeName "colSpan") . show

rowSpan :: Int -> Attr i
rowSpan = attr (attributeName "rowSpan") . show

content :: String -> Attr i
content = attr $ attributeName "content"

for :: String -> Attr i
for = attr $ attributeName "htmlFor"

height :: Int -> Attr i
height = attr (attributeName "height") . show

href :: String -> Attr i
href = attr $ attributeName "href"

httpEquiv :: String -> Attr i
httpEquiv = attr $ attributeName "http-equiv"

id_ :: String -> Attr i
id_ = attr $ attributeName "id"

name :: String -> Attr i
name = attr $ attributeName "name"

rel :: String -> Attr i
rel = attr $ attributeName "rel"

src :: String -> Attr i
src = attr $ attributeName "src"

target :: String -> Attr i
target = attr $ attributeName "target"

title :: String -> Attr i
title = attr $ attributeName "title"

type_ :: String -> Attr i
type_ = attr $ attributeName "type"

value :: String -> Attr i
value = attr $ attributeName "value"

width :: Int -> Attr i
width = attr (attributeName "width") . show

disabled :: Bool -> Attr i
disabled = attr $ attributeName "disabled"

required :: Bool -> Attr i
required = attr $ attributeName "required"

readonly :: Bool -> Attr i
readonly = attr $ attributeName "readonly"

spellcheck :: Bool -> Attr i
spellcheck = attr $ attributeName "spellcheck"

enabled :: Bool -> Attr i
enabled = disabled . not

checked :: Bool -> Attr i
checked = attr $ attributeName "checked"

selected :: Bool -> Attr i
selected = attr $ attributeName "selected"

placeholder :: String -> Attr i
placeholder = attr $ attributeName "placeholder"
