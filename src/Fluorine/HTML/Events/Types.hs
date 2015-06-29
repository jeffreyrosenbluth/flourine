-------------------------------------------------------------------------------
-- |
-- Module      :  Fluorine.Attributes
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- This module defines types for common DOM events
------------------------------------------------------------------------------

module Fluorine.HTML.Events.Types where

-- XXX import Data.DOM.Simple.Types

-- | This record synonym captures the properties which appear on every DOM event.
-- |
-- | The `fields` type parameter allows us to attach different types of additional
-- | properties to represent more specific types of events.
data Event f = Event
  { bubbles :: Bool
  , cancelable :: Bool
  , currentTarget :: String -- XXX HTMLElement
  , target :: String -- XXX HTMLElement
  , timeStamp :: Int
  , eType :: String
  , fields :: [f]
  }

-- | Identifies the additional fields which are available on mouse events.
data MouseEvent = MouseEvent
  { buttonM :: Int
  , detailM :: Int
  , relatedTargetM :: String -- XXX HTMLElement
  , clientXM :: Int
  , clientYM :: Int
  , screenXM  :: Int
  , screenYM  :: Int
  , ctrlKeyM  :: Bool
  , shiftKeyM :: Bool
  , altKeyM :: Bool
  , metaKeyM  :: Bool
  , whichM :: Int
  }

-- | Identifies the additional fields which are available on keyboard events.
data KeyboardEvent = KeyboardEvent
  { charCodeK :: Int
  , keyCodeK :: Int
  , ctrlKeyK  :: Bool
  , shiftKeyK :: Bool
  , altKeyK :: Bool
  , metaKeyK  :: Bool
  , whichK :: Int
  }

-- | Identifies the additional fields which are available on focus events.
data FocusEvent = FocusEvent
  { relatedTargetF :: String -- XXX HTMLElement
  }
