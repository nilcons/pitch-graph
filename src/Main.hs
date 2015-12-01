module Main (main) where

import           Control.Monad (forever)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString, doubleLE)
import qualified Data.ByteString.Lazy as BL
import           DywaPitchTrack
import           Sound.Pulse.Simple
import           System.IO

f2d :: Float -> Double
f2d = realToFrac

calcPitch :: [Float] -> PitchTrack Double
calcPitch fs = computePitch ds
  where
    ds = BL.toStrict $ toLazyByteString $ foldMap (doubleLE . f2d) fs

main :: IO ()
main = do
  pulse <- simpleNew Nothing "pitch-graph" Record Nothing "Pitch Graph"
           (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
  runPitchTrack 2048 $ forever $ do
    s <- liftIO $ simpleRead pulse 2048
    pitch <- calcPitch s
    liftIO $ do putStr $ "\r\ESC[K" ++ show pitch
                hFlush stdout
  simpleFree pulse
