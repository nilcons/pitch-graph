module PitchGraph.Sound (
  pulseMain,
  ) where

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

pulseLoop :: Pulse.Simple -> ([Double] -> IO ()) -> PitchTrack ()
pulseLoop pulse callBack = loop (replicate 500 440)
  where
    loop ps = do
      s <- liftIO $ simpleRead pulse winSize
      pitch <- calcPitch s
      -- liftIO $ do putStr $ "\r\ESC[K" ++ show pitch
      --             hFlush stdout
      let ps' = take 1000 $ pitch : ps
      liftIO $ callBack ps'
      loop ps'

pulseMain :: ([Double] -> IO ()) -> IO ()
pulseMain callBack = do
  pulse <- simpleNew Nothing "pitch-graph" Record Nothing "Pitch Graph"
           (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
  runPitchTrack winSize $ pulseLoop pulse callBack
  simpleFree pulse
