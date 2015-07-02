-------------------------------------------------------------------------------
-- |
-- Module      :  Fluorine.Attributes
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- This module defines well-typed wrappers for common DOM events, so that
-- they may be safely embedded in HTML documents.
-------------------------------------------------------------------------------

module Fluorine.HTML.Events
  ( input
  , input_
  , onAbort
  , onBeforeUnload
  , onError
  , onHashChange
  , onLoad
  , onPageShow
  , onPageHide
  , onResize
  , onScroll
  , onUnload
  , onChange
  , onInvalid
  , onReset
  , onSearch
  , onSelect
  , onSubmit
  , onClick
  , onContextMenu
  , onDoubleClick
  , onMouseDown
  , onMouseEnter
  , onMouseLeave
  , onMouseMove
  , onMouseOver
  , onMouseOut
  , onMouseUp
  , onKeyDown
  , onKeyPress
  , onKeyUp
  , onBlur
  , onFocus
  , onFocusIn
  , onFocusOut
  ) where

import           Fluorine.HTML.Events.Handler
import           Fluorine.HTML.Events.Types
import qualified Fluorine.HTML.Attributes as H

-- | A helper function which can be used to create simple event handlers.
-- |
-- | Often we don't need to use `EventHandler` or the monad underlying our component, and just need
-- | to generate an input to the signal function.
-- |
-- | This function provides an alternative to making two nested calls to `pure`:
-- |
-- | ```purescript
-- | onClick (input \_ -> Input)
-- | ```
input :: Applicative m => (a -> i) -> a -> EventHandler (m i)
input f e = pure (pure (f e))

-- | A helper function for simple event handlers that provide an input to the signal function,
-- | where there is no need to make use of the event value to generate the input.
-- |
-- | ```purescript
-- | onclick (input_ Input)
-- | ```
input_ :: Applicative m => i -> a -> EventHandler (m i)
input_ x _ = pure (pure x)

onAbort  :: (Event () -> EventHandler i) -> H.Attr i
onAbort = H.handler (H.eventName "abort")

onBeforeUnload :: (Event () -> EventHandler i) -> H.Attr i
onBeforeUnload = H.handler (H.eventName "beforeunload")

onError :: (Event () -> EventHandler i) -> H.Attr i
onError = H.handler (H.eventName "error")

onHashChange :: (Event () -> EventHandler i) -> H.Attr i
onHashChange = H.handler (H.eventName "hashchange")

onLoad :: (Event () -> EventHandler i) -> H.Attr i
onLoad = H.handler (H.eventName "load")

onPageShow :: (Event () -> EventHandler i) -> H.Attr i
onPageShow = H.handler (H.eventName "pageshow")

onPageHide :: (Event () -> EventHandler i) -> H.Attr i
onPageHide = H.handler (H.eventName "pagehide")

onResize :: (Event () -> EventHandler i) -> H.Attr i
onResize = H.handler (H.eventName "resize")

onScroll :: (Event () -> EventHandler i) -> H.Attr i
onScroll = H.handler (H.eventName "scroll")

onUnload :: (Event () -> EventHandler i) -> H.Attr i
onUnload = H.handler (H.eventName "unload")

onChange :: (Event () -> EventHandler i) -> H.Attr i
onChange = H.handler (H.eventName "change")

onInvalid :: (Event () -> EventHandler i) -> H.Attr i
onInvalid = H.handler (H.eventName "invalid")

onReset :: (Event () -> EventHandler i) -> H.Attr i
onReset = H.handler (H.eventName "reset")

onSearch :: (Event () -> EventHandler i) -> H.Attr i
onSearch = H.handler (H.eventName "search")

onSelect :: (Event () -> EventHandler i) -> H.Attr i
onSelect = H.handler (H.eventName "select")

onSubmit :: (Event () -> EventHandler i) -> H.Attr i
onSubmit = H.handler (H.eventName "submit")

onClick :: (Event MouseEvent -> EventHandler i) -> H.Attr i
onClick = H.handler (H.eventName "click")

onContextMenu :: (Event MouseEvent -> EventHandler i) -> H.Attr i
onContextMenu = H.handler (H.eventName "contextmenu")

onDoubleClick :: (Event MouseEvent -> EventHandler i) -> H.Attr i
onDoubleClick = H.handler (H.eventName "dblclick")

onMouseDown :: (Event MouseEvent -> EventHandler i) -> H.Attr i
onMouseDown = H.handler (H.eventName "mousedown")

onMouseEnter :: (Event MouseEvent -> EventHandler i) -> H.Attr i
onMouseEnter = H.handler (H.eventName "mouseenter")

onMouseLeave :: (Event MouseEvent -> EventHandler i) -> H.Attr i
onMouseLeave = H.handler (H.eventName "mouseleave")

onMouseMove :: (Event MouseEvent -> EventHandler i) -> H.Attr i
onMouseMove = H.handler (H.eventName "mousemove")

onMouseOver :: (Event MouseEvent -> EventHandler i) -> H.Attr i
onMouseOver = H.handler (H.eventName "mouseover")

onMouseOut :: (Event MouseEvent -> EventHandler i) -> H.Attr i
onMouseOut = H.handler (H.eventName "mouseout")

onMouseUp :: (Event MouseEvent -> EventHandler i) -> H.Attr i
onMouseUp = H.handler (H.eventName "mouseup")

onKeyDown :: (Event KeyboardEvent -> EventHandler i) -> H.Attr i
onKeyDown = H.handler (H.eventName "keydown")

onKeyPress :: (Event KeyboardEvent -> EventHandler i) -> H.Attr i
onKeyPress = H.handler (H.eventName "keypress")

onKeyUp :: (Event KeyboardEvent -> EventHandler i) -> H.Attr i
onKeyUp = H.handler (H.eventName "keyup")

onBlur :: (Event FocusEvent -> EventHandler i) -> H.Attr i
onBlur = H.handler (H.eventName "blur")

onFocus :: (Event FocusEvent -> EventHandler i) -> H.Attr i
onFocus = H.handler (H.eventName "focus")

onFocusIn :: (Event FocusEvent -> EventHandler i) -> H.Attr i
onFocusIn = H.handler (H.eventName "focusin")

onFocusOut :: (Event FocusEvent -> EventHandler i) -> H.Attr i
onFocusOut = H.handler (H.eventName "focusout")
