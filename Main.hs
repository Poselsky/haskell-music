module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import System.Process
import Text.Printf
import Data.List
import GHC.Float (int2Float)

type Pulse = Float
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beats = Float

outputFilePath :: FilePath
outputFilePath = "output.bin"

volume :: Float
volume = 0.3

sampleRate :: Samples
sampleRate = 4800.0

pitchStandard :: Hz
pitchStandard = 440.0

bpm :: Beats
bpm = 90.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

-- NOTE: the formula is taken from https://pages.mtu.edu/~suits/NoteFreqCalcs.html
f :: Semitones -> Hz
f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

note :: Semitones -> Beats -> [Pulse]
note n beats = freq (f n) (beats * beatDuration)

freq :: Hz -> Seconds -> [Pulse]
freq hz duration =
  map (* volume) $ zipWith (\x y -> x * y) output adsr
  where
    step = (hz * 2 * pi) / sampleRate

    output :: [Pulse]
    output = map sin $ map (* step) [0.0 .. sampleRate * duration]

    -- Ratio and angle
    adsrRatio:: Int -> (Float, Float)
    adsrRatio index = (width !! index, atan $ ((height !! (index + 1)) - (height !! index)) / ((width !! (index + 1)) - (width !! (index))))
      where
        width = zipWith (\x len -> (x / len):: Float) [15,10,55,20,0] $ repeat $ int2Float outputLength
        height = [0.0,1.0, 0.6, 0.6, 0]

    outputLength:: Int
    outputLength = length output

    adsr:: [Float]
    adsr = fst $ foldr foldHelper ([], 0.0) $ zip [0..] $ take (outputLength) $ repeat 1
      where
        foldHelper:: (Int, Float) -> ([Float], Float) -> ([Float], Float)
        foldHelper (ind, val) (accList, previousVal)
          | index <= (fst $ adsrRatio 0)                                 = ([previousVal + (pointStep * (sin $ snd $ adsrRatio 0))] ++ accList, previousVal + (pointStep * (sin $ snd $ adsrRatio 0)))
          | index >  (fst $ adsrRatio 0) && index <= (fst $ adsrRatio 1) = ([previousVal + (pointStep * (sin $ snd $ adsrRatio 1))] ++ accList, previousVal + (pointStep * (sin $ snd $ adsrRatio 1)))
          | index >  (fst $ adsrRatio 1) && index <= (fst $ adsrRatio 2) = ([previousVal + (pointStep * (sin $ snd $ adsrRatio 2))] ++ accList, previousVal + (pointStep * (sin $ snd $ adsrRatio 2)))
          | index >  (fst $ adsrRatio 2)                                 = ([previousVal + (pointStep * (sin $ snd $ adsrRatio 3))] ++ accList, previousVal + (pointStep * (sin $ snd $ adsrRatio 3)))
            where 
              index = int2Float ind
              pointStep = 1 / (int2Float (outputLength))


adsr:: [Float]
adsr = fst $ foldr foldHelper ([], 0.0) $ zip [0..] $ take 10 $ repeat 1
    where
      foldHelper:: (Int, Float) -> ([Float], Float) -> ([Float], Float)
      foldHelper (ind, val) (accList, previousVal)
        | index <= (fst $ adsrRatio 0)                                 = ([previousVal + (snd $ adsrRatio 0)] ++ accList, previousVal + (snd $ adsrRatio 0))
        | index >  (fst $ adsrRatio 0) && index <= (fst $ adsrRatio 1) = ([previousVal - (snd $ adsrRatio 1)] ++ accList, previousVal - (snd $ adsrRatio 1))
        | index >  (fst $ adsrRatio 1) && index <= (fst $ adsrRatio 2) = ([0.60] ++ accList,0.60)
        | index >  (fst $ adsrRatio 2)                                 = ([previousVal - (snd $ adsrRatio 2)] ++ accList, previousVal - (snd $ adsrRatio 2))
          where index = int2Float ind
      -- Ratio and steepnes
      adsrRatio:: Int -> (Float, Float)
      adsrRatio index = (adsrRatio' !! index, (adsrRatio' !! (index + 1)) / ((adsrRatio' !! (index + 1)) - (adsrRatio' !! (index))))
        where
          adsrRatio' = zipWith (\x len -> (x / len):: Float) [25,15,40,20] $ repeat $ int2Float 10

awaken :: [Pulse]
awaken =
  concat
    [ 
        noteF5 0.5
      , noteF5 0.25
      , noteF5 0.25
      , noteF5 0.25
      , noteD5Eb5 0.25
      , noteF5 0.25
      , noteF5Gb5 0.25
      , noteG5Ab5 0.25
      , noteA5 0.25
      , noteG5Ab5 0.25
      , noteF5Gb5 0.25
      , noteF5 0.5
      , noteF5 0.5

      , noteZero 0.5

      , noteF5 0.5
      , noteF5 0.25
      , noteF5 0.25
      , noteF5 0.25
      , noteD5Eb5 0.25
      , noteF5 0.25
      , noteF5Gb5 0.25
      , noteG5Ab5 0.25
      , noteA5 0.25
      , noteG5Ab5 0.25
      , noteF5Gb5 0.25
      , noteF5 0.5
      , noteD5Eb5 0.5

      , noteZero 0.5

      , noteF5 0.25
      , noteF5 0.25
      , noteF5 0.25
      , noteD5Eb5 0.25
      , noteF5 0.25
      , noteF5Gb5 0.25
      , noteG5Ab5 0.25
      , noteA5 0.25
      , noteG5Ab5 0.25
      , noteF5Gb5 0.25
      , noteF5 0.5
      , noteF5 0.5
      , noteF5 0.25
      , noteF5 0.5
      , noteF5Gb5 0.25
      , noteF5Gb5 0.5
      , noteG5Ab5 0.25
      , noteG5Ab5 0.25
      , noteZero 0.25
      , noteA5 0.25
      , noteA5 0.5
      , noteC5 0.25
      , noteA5 0.25
      , noteG5Ab5 0.25
      , noteF5Gb5 0.25

    ]

hehehe :: [Pulse]
hehehe = concat [ note 0 0.25
                , note 0 0.25
                , note 12 0.5
                , note 7 0.75
                , note 6 0.5
                , note 5 0.5
                , note 3 0.5
                , note 0 0.25
                , note 3 0.25
                , note 5 0.25
                , note 0 0.25
                , note 0 0.25
                , note 12 0.5
                , note 7 0.75
                , note 6 0.5
                , note 5 0.5
                , note 3 0.5
                , note 0 0.25
                , note 3 0.25
                , note 5 0.25
                ]

save :: FilePath -> IO ()
save filePath = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE awaken

play :: IO ()
play = do
  save outputFilePath
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

main :: IO ()
main = save outputFilePath





noteZero :: Seconds -> [Pulse]
noteZero beats = freq 0 (beats * beatDuration)


noteF5:: Seconds -> [Pulse]
noteF5 beats = freq (f 8) (beats * beatDuration)

noteD5Eb5:: Seconds -> [Pulse]
noteD5Eb5 beats = freq (f 7) (beats * beatDuration)

noteF5Gb5:: Seconds -> [Pulse]
noteF5Gb5 beats = freq (f 9) (beats * beatDuration)

noteG5Ab5:: Seconds -> [Pulse]
noteG5Ab5 beats = freq (f 11) (beats * beatDuration)

noteA5:: Seconds -> [Pulse]
noteA5 beats = freq (f 12) (beats * beatDuration)

noteC5:: Seconds -> [Pulse]
noteC5 beats = freq (f 15) (beats * beatDuration)
