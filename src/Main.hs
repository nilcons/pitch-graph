{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main (main) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM
import qualified Graphics.UI.Gtk as Gtk
import           System.Posix.Signals

import           PitchGraph.Chart
import           PitchGraph.Sound

main :: IO ()
main = do
  Gtk.unsafeInitGUIForThreadedRTS
  pausedVar <- newTVarIO False
  (_, pitchCall) <- setupChartWindow 640 480 pausedVar
  forkIO $ pulseMain pitchCall pausedVar
  -- Properly handle Ctrl-C:
  installHandler sigINT (Catch $ Gtk.postGUIAsync $ Gtk.mainQuit) Nothing
  Gtk.mainGUI
