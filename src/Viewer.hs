module Viewer where

import Control.Lens
import Data.IORef

import Types
import GraphicsContext

viewerStart :: Options -> Data -> IORef GraphicsContext -> IO ()
viewerStart opts d gc = do
  let v = Viewer opts d gc
  putStrLn "Starting viewer..."
  gcLoop (v ^. viewGCRef)
  putStrLn "Viewer closed."
