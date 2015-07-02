-------------------------------------------------------------------------------
-- |
-- Module      :  Fluorine
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
-------------------------------------------------------------------------------

module Fluorine
  ( Driver()
  , Process()
  , mainLoop
  ) where

import Control.Monad
import Data.IORef
import Fluorine.Signal
import Fluorine.HTML.Events.Types

-------------------------------------------------------------------------------

type Driver i = i -> IO ()

-- | `runUI` renders a `Component` to the DOM using `virtual-dom`.
-- |
-- | This function is the workhorse of the Halogen library. It can be called in `main`
-- | to set up the application and create the driver function, which can be used to
-- | send inputs to the UI from external components.
-- runUI :: Component (Event (HalogenEffects eff)) req req
--       -> Eff (HalogenEffects eff) (Tuple HTMLElement (Driver req eff))
-- runUI sf = sf `runUIWith` \_ _ _ -> return unit

-- | A variant of `runUI` which supports a _post-render hook_. This allows applications
-- | to support third-party components or other custom behaviors by modifying the DOM after
-- | each update.
-- |
-- | This is considered an advanced feature, and should only be used with an understanding of
-- | the rendering pipeline.
-- runUIWith :: Component (Event (HalogenEffects eff)) req req
--           -> (req -> HTMLElement -> Driver req eff -> Eff (HalogenEffects eff) Unit)
--           -> Eff (HalogenEffects eff) (Tuple HTMLElement (Driver req eff))
-- runUIWith sf postRender = mainLoop (pure <<< componentProcess sf postRender)

-- | A `Process` receives inputs and outputs effectful computations which update the DOM.
type Process req = SF (req, HTMLElement) (IO HTMLElement)

-- | This function provides the low-level implementation of Halogen's DOM update loop.
-- |
-- | The first argument is a function which receives the `Driver` function as an argument and
-- | constructs a `Process` which will update the DOM given an input.
-- |
-- | This function could be reused to create other types of applications based on signal functions
-- | (2D and 3D canvas, text-based, etc.)
mainLoop :: (Driver req -> IO (HTMLElement, Process req))
         -> IO (HTMLElement, Driver req)
mainLoop buildProcess = do
  ref <- newIORef Nothing
  go ref
  where
    go ref = do
      (node, process) <- buildProcess driver
      writeIORef ref $ Just (process, node)
      return (node, driver)
      where
        driver req = void $ {- setTimeout globalWindow 0 $-} do
          ms <- readIORef ref
          case ms of
            Just (process, node) -> do
              let work = runSF process (req, node)
              node' <- result work
              writeIORef ref $ Just (next work, node')
            Nothing -> error "Error: An attempt to re-render was made during the initial render."
