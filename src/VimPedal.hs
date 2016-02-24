{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Data.Function (on)
import Data.List     (maximumBy,minimumBy,sortBy)
import Data.Word     (Word8)
import Foreign.Marshal.Array
import Foreign.Ptr

import System.Console.CmdArgs.Implicit
import System.Process (callCommand)

import Sound.ALSA.PCM
import Sound.ALSA.PCM.Node.ALSA

main :: IO ()
main = do
    cfg <- cmdArgs configOptions
    let source = alsaSoundSource (soundSource cfg) inputFormat
    allocaArray bufferSize $ \buffer ->
        withSoundSource source (listenOn cfg source buffer)

configOptions = VimPedal {
      soundSource  = "default"
    , minThreshold = 70  &= typ "0-255"
    , maxThreshold = 180 &= typ "0-255"
    , pressKey     = "i"
    , depressKey   = "Escape"
    } &= summary "VimPedal"

data VimPedal =
    VimPedal {
      soundSource  :: String
    , minThreshold :: Int
    , maxThreshold :: Int
    , pressKey     :: String
    , depressKey   :: String
    } deriving (Show, Data, Typeable)

xdokey :: String
xdokey  = "xdotool key "

bufferSize :: Int
bufferSize = 4096

sampleRate :: SampleFreq
sampleRate = 8000

inputFormat :: SoundFmt Word8
inputFormat = SoundFmt sampleRate 

listenOn :: VimPedal -> SoundSource Pcm Word8 -> Ptr Word8 -> Pcm Word8 -> IO ()
listenOn cfg source buffer handle = do
    n       <- soundSourceRead source handle buffer bufferSize
    pcmData <- peekArray n buffer
    detectPedalPress cfg pcmData
    listenOn cfg source buffer handle

detectPedalPress :: VimPedal -> [Word8] -> IO ()
detectPedalPress cfg intvSamples = mapM_ runCommandOnThreshold $ sortBy (compare `on` snd) [mx,mi]
        where ps = filter (even . snd) (zip intvSamples [0..])
              mx = maximumBy (compare `on` fst) ps
              mi = minimumBy (compare `on` fst) ps
              runCommandOnThreshold (v,_)
                | v < fromIntegral (minThreshold cfg) = callCommand $ xdokey ++ (pressKey cfg)
                | v > fromIntegral (maxThreshold cfg) = callCommand $ xdokey ++ (depressKey cfg)
                | otherwise = return ()
