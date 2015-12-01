{-# LANGUAGE OverloadedStrings #-}

module PitchGraph.Chart (
  setupChartWindow,
  PitchUpdateCall,
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad (forever)
import           Data.Default
import           Data.Maybe
import           Graphics.Rendering.Chart as Ch
import qualified Graphics.Rendering.Chart.Backend.Cairo as Ch
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as GE
import           System.IO.Unsafe

type PitchUpdateCall = [Double] -> IO ()

-- TODO(klao): move inside
pausedVar :: TVar Bool
pausedVar = unsafePerformIO $ newTVarIO False
{-# NOINLINE pausedVar #-}

mainHandleKeys :: Gtk.Window -> GE.Event -> IO Bool
mainHandleKeys window (GE.Key {GE.eventKeyName=key})
  | key `elem` ["q", "Q", "Escape"] = Gtk.widgetDestroy window >> return True
  | key == "space"                  = togglePaws >> return True
  | otherwise                       = return True -- traceShow ("key: " <> key) return True
  where
    togglePaws = atomically $ do p <- readTVar pausedVar
                                 writeTVar pausedVar (not p)
mainHandleKeys _ _ = return False

setupChartWindow :: Int -> Int -> IO (Gtk.Window, PitchUpdateCall)
setupChartWindow windowWidth windowHeight = do
    window <- Gtk.windowNew
    (canvas, updCall) <- createChartCanvas
    Gtk.widgetSetSizeRequest window windowWidth windowHeight
    _ <- Gtk.onKeyPress window $ mainHandleKeys window
    Gtk.set window [Gtk.containerChild Gtk.:= canvas]
    _ <- Gtk.onDestroy window Gtk.mainQuit
    Gtk.widgetShowAll window
    return (window, updCall)

createChartCanvas :: IO (Gtk.DrawingArea, PitchUpdateCall)
createChartCanvas = do
  pitchesVar <- newTVarIO $ replicate 10000 440.0
  (canvas, expose) <- lowLevelChartCanvas $ \width _ -> do
    series <- atomically $ readTVar pitchesVar
    let numDots = fitsDots width
        pitches = reverse $ take numDots series
    return $ layoutFromPitchList pitches

  return (canvas, \pitchList -> do
             atomically $ writeTVar pitchesVar pitchList
             expose)

layoutFromPitchList :: [Double] -> PitchLayout
layoutFromPitchList pitches
  = def & layout_plots .~ [plot]
    & layout_y_axis .~ semitoneAxis
    & layout_right_axis_visibility .~ def
    & layout_x_axis .~ xAxis
  where
    plot = toPlot $ def & plot_points_values .~ toSemitoneList pitches
           & plot_points_style.point_radius .~ 3
    semitoneAxis = def & laxis_generate .~ const (makeAxis soundName (l, l, l))
    l = [-12..12]               -- TODO(klao): make this adjustable
    xAxis = def & laxis_generate .~ const (makeAxis show (xl, xl, xl))
    xl = [0, 100, 200, 300]

toSemitoneList :: [Double] -> [(Int, Double)]
toSemitoneList = catMaybes . zipWith (\i p -> pitchToSemitone' i p) [0..]

pitchToSemitone' :: a -> Double -> Maybe (a, Double)
pitchToSemitone' x p | p < 20 = Nothing
                     | otherwise = Just (x, logBase 2 p * 12 - c)
  where
    c = logBase 2 220 * 12 - 9

soundName :: Double -> String
soundName semitone = sound ++ octave
  where
    st = round semitone :: Int
    octave = show $ st `div` 12 + 4
    sound = case st `mod` 12 of
      0  -> "C"
      1  -> "C#"
      2  -> "D"
      3  -> "D#"
      4  -> "E"
      5  -> "F"
      6  -> "F#"
      7  -> "G"
      8  -> "G#"
      9  -> "A"
      10 -> "A#"
      11 -> "B"
      _ -> "X"

--------------------------------------------------------------------------------

writeTMVar :: TMVar a -> a -> STM ()
writeTMVar var x = tryTakeTMVar var >> putTMVar var x


-- marginPixels, dotWidth :: Int
-- marginPixels = 100
-- dotWidth = 6

showDots :: Int
showDots = 300

fitsDots :: Int -> Int
-- fitsDots width = (width - marginPixels) `div` dotWidth `div` 100 * 100
-- Fixed size for now:
fitsDots _ = showDots

type RedrawCB = IO ()
type PitchLayout = Layout Int Double
type LayoutCB = Int -> Int -> IO PitchLayout  -- width -> height -> layout

lowLevelChartCanvas :: LayoutCB -> IO (Gtk.DrawingArea, RedrawCB)
lowLevelChartCanvas layoutf = do
  canvas <- Gtk.drawingAreaNew
  cachedRenderable <- newEmptyTMVarIO
  userReqVar <- newTVarIO False
  sizeVar <- newTVarIO ((-1,-1), (-1,-1))

  _ <- forkIO $ forever $ do
    (w,h) <- atomically $ do
      sizes <- readTVar sizeVar
      let newSize = snd sizes
      check $ newSize /= (-1,-1)
      userReq <- readTVar userReqVar
      check $ userReq || uncurry (/=) sizes
      writeTVar userReqVar False
      writeTVar sizeVar (newSize, newSize)
      return $ snd sizes
    newLayout <- layoutf w h
    -- deepEvalLayout newLayout
    atomically $ writeTMVar cachedRenderable $ toRenderable newLayout
    Gtk.postGUIAsync $ Gtk.widgetQueueDraw canvas
    threadDelay 70000

  _ <- Gtk.onExposeRect canvas $
       \_rect -> do
         csize <- Gtk.widgetGetSize canvas
         atomically $ modifyTVar sizeVar $ _2 .~ csize
         chart <- atomically $ readTMVar cachedRenderable
         drawChart canvas chart
         return ()

  return (canvas, atomically $ writeTVar userReqVar True)

drawChart :: Gtk.DrawingArea -> Ch.Renderable a -> IO ()
drawChart canvas chart = do
  win <- Gtk.widgetGetDrawWindow canvas
  (width, height) <- Gtk.widgetGetSize canvas
  region <- Gtk.regionRectangle $ Gtk.Rectangle 0 0 width height
  let (w,h) = (fromIntegral width,fromIntegral height)
  Gtk.drawWindowBeginPaintRegion win region
  _ <- Gtk.renderWithDrawable win $ do
    Ch.runBackend (Ch.defaultEnv Ch.bitmapAlignmentFns) (Ch.render chart (w,h))
  Gtk.drawWindowEndPaint win
