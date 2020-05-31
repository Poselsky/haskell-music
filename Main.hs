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
bpm = 160

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
        width = zipWith (\x len -> (x / len):: Float) [15,5,60,20,0] $ repeat $ int2Float outputLength
        height = [0.0,1.0, 0.75, 0.75, 0]

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

testingBeats:: [Pulse]
testingBeats = zipWith (+) (map (0.7*)firstPart) (map (0.3*) secondPart)
  where
    firstPart = concat [
                        noteZero 0.5
                        , noteG4 0.25
                        , noteG4 0.5
                        , noteG4 0.5
                        , noteF4 0.5
                        , noteG4 0.5
                        , noteA4 0.5
                        , noteB4 0.5
                        , noteB4 0.5
                        , noteA4Bb4 0.5
                        , noteA4 0.5
                        , noteG4 0.25
                        , noteZero 0.25
                        , noteZero 0.5
                        , noteG4 0.125
                        , noteZero 0.125
                        
                        , noteZero 0.5
                        , noteZero 0.25
                        
                        , noteG4 0.125
                        , noteZero 0.125
                        , noteG4 0.25
                        , noteZero 0.25
                        , noteG4 0.25
                        , noteZero 0.25
                        , noteF4 0.25
                        , noteZero 0.25
                        , noteG4 0.25
                        , noteZero 0.25
                        , noteA4 0.25
                        , noteZero 0.25
                        , noteB4 0.25
                        , noteZero 0.25
                        , noteB4 0.25
                        , noteZero 0.25
                        , noteA4Bb4 0.25
                        , noteZero 0.25
                        , noteA4 0.25
                        , noteZero 0.25
                        , noteG4 0.25
                        , noteZero 0.25
                        , noteZero 0.5
                        , noteF4 0.125
                        , noteZero 1.25

                        , noteZero 0.5
                        , noteG4 0.125
                        , noteZero 0.125
                        , noteG4 0.25
                        , noteZero 0.25
                        , noteG4 0.25
                        , noteZero 0.25
                        , noteF4 0.25
                        , noteZero 0.25
                        , noteG4 0.25
                        , noteZero 0.25
                        , noteA4 0.25
                        , noteZero 0.25
                        , noteB4 0.25
                        , noteZero 0.25
                        , noteB4 0.25
                        , noteZero 0.25
                        , noteA4Bb4 0.25
                        , noteZero 0.25
                        , noteA4 0.25
                        , noteZero 0.25
                        , noteG4 0.25
                        , noteZero 0.25
                        , noteZero 0.5
                        , noteG4 0.125
                        , noteZero 0.125
                        

                        , noteZero 0.5

                        , noteG4 0.25
                        , noteZero 0.25
                        , noteA4 0.25
                        , noteZero 0.25
                        , noteA4 0.25
                        , noteZero 0.25
                        , noteB4 0.125
                        , noteZero 0.125
                        , noteB4 0.25
                        , noteZero 0.25
                        , noteB4 0.25
                        , noteZero 0.25
                        , noteZero 0.25
                        , noteD5 0.25
                        , noteZero 0.25
                        , noteB4 0.25
                        , noteZero 0.25
                        , noteA4Bb4 0.25
                        , noteZero 0.25

                        , noteA4 0.25
                        , noteZero 0.25
                      ]
    secondPart = concat [
                          noteZero 0.25
                          , noteG4 3
                          , noteG5 0.25
                          , noteG5 0.25

                          , noteD5 3
                          , noteE5 0.25
                          , noteE5 0.25
                          , noteD5 3

                          , noteB4 0.25
                          , noteB4 0.25
                          , noteA4Bb4 3

                          , noteA4 0.25
                          , noteA4 0.25

                          , noteG4 3
                          , noteG5 0.25
                          , noteG5 0.25

                          , noteD5 3
                          , noteE5 0.25
                          , noteE5 0.25
                          , noteD5 3

                          , noteB4 0.25
                          , noteB4 0.25
                          , noteA4Bb4 3

                          , noteA4 0.25
                          , noteA4 0.25

                          , repeat 0
                        ]

save :: FilePath -> IO ()
save filePath = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE testingBeats

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
noteC5 beats = freq (f 3) (beats * beatDuration)

noteG5:: Seconds -> [Pulse]
noteG5 beats = freq (f 10) (beats * beatDuration)


noteD5:: Seconds -> [Pulse]
noteD5 beats = freq (f 5) (beats * beatDuration)

noteE5:: Seconds -> [Pulse]
noteE5 beats = freq (f 7) (beats * beatDuration)

noteC5Db5:: Seconds -> [Pulse]
noteC5Db5 beats = freq (f 4) (beats * beatDuration)



noteF4:: Seconds -> [Pulse]
noteF4 beats = freq (f (-4)) (beats * beatDuration)

noteD4Eb4:: Seconds -> [Pulse]
noteD4Eb4 beats = freq (f (-6)) (beats * beatDuration)

noteF4Gb4:: Seconds -> [Pulse]
noteF4Gb4 beats = freq (f (-3)) (beats * beatDuration)

noteG4Ab4:: Seconds -> [Pulse]
noteG4Ab4 beats = freq (f (-1)) (beats * beatDuration)

noteA4:: Seconds -> [Pulse]
noteA4 beats = freq (f 0) (beats * beatDuration)

noteB4:: Seconds -> [Pulse]
noteB4 beats = freq (f 2) (beats * beatDuration)

noteG4:: Seconds -> [Pulse]
noteG4 beats = freq (f (-2)) (beats * beatDuration)

noteC4:: Seconds -> [Pulse]
noteC4 beats = freq (f (-9)) (beats * beatDuration)


noteA4Bb4:: Seconds -> [Pulse]
noteA4Bb4 beats = freq (f 1) (beats * beatDuration)