module PitchGraph.Sound (
  pulseMain,
  ) where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString.Builder (toLazyByteString, doubleLE)
import qualified Data.ByteString.Lazy as BL
import           DywaPitchTrack
import           Sound.Pulse.Simple as Pulse

calcPitch :: [Float] -> PitchTrack Double
calcPitch fs = computePitch ds
  where
    ds = BL.toStrict $ toLazyByteString $ foldMap (doubleLE . realToFrac) fs

winSize :: Int
winSize = 4096

pulseLoop :: Pulse.Simple -> ([Double] -> IO ()) -> TVar Bool -> PitchTrack ()
pulseLoop pulse callBack pausedVar = loop (replicate 500 220)
  where
    loop ps = do
      liftIO $ atomically $ do paused <- readTVar pausedVar
                               check $ not paused
      s <- liftIO $ simpleRead pulse winSize
      pitch <- calcPitch s
      -- liftIO $ do putStr $ "\r\ESC[K" ++ show pitch
      --             hFlush stdout
      -- liftIO $ putStr "." >> hFlush stdout
      let ps' = take 1000 $ pitch : ps
      liftIO $ callBack ps'
      loop ps'

pulseMain :: ([Double] -> IO ()) -> TVar Bool -> IO ()
pulseMain callBack pausedVar = do
  let buf = BufferAttr Nothing Nothing Nothing Nothing (Just $ 4 * winSize)
  pulse <- simpleNew Nothing "pitch-graph" Record Nothing "Pitch Graph"
           (SampleSpec (F32 LittleEndian) 44100 1) Nothing (Just buf)
  runPitchTrack winSize $ pulseLoop pulse callBack pausedVar
  simpleFree pulse
