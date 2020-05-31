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
volume = 0.2

sampleRate :: Samples
sampleRate = 4800.0

pitchStandard :: Hz
pitchStandard = 440.0

bpm :: Beats
bpm = 120.0

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
        width = zipWith (\x len -> (x / len):: Float) [20,10,50,20,0] $ repeat $ int2Float $ length output
        height = [0.0,1.0, 0.6, 0.6, 0]


    adsr:: [Float]
    adsr = fst $ foldr foldHelper ([], 0.0) $ zip [0..] $ take (length output) $ repeat 1
      where
        foldHelper:: (Int, Float) -> ([Float], Float) -> ([Float], Float)
        foldHelper (ind, val) (accList, previousVal)
          | index <= (fst $ adsrRatio 0)                                 = ([previousVal + (pointStep * (sin $ snd $ adsrRatio 0))] ++ accList, previousVal + (pointStep * (sin $ snd $ adsrRatio 0)))
          | index >  (fst $ adsrRatio 0) && index <= (fst $ adsrRatio 1) = ([previousVal + (pointStep * (sin $ snd $ adsrRatio 1))] ++ accList, previousVal + (pointStep * (sin $ snd $ adsrRatio 1)))
          | index >  (fst $ adsrRatio 1) && index <= (fst $ adsrRatio 2) = ([previousVal + (pointStep * (sin $ snd $ adsrRatio 2))] ++ accList, previousVal + (pointStep * (sin $ snd $ adsrRatio 2)))
          | index >  (fst $ adsrRatio 2)                                 = ([previousVal + (pointStep * (sin $ snd $ adsrRatio 3))] ++ accList, previousVal + (pointStep * (sin $ snd $ adsrRatio 3)))
            where 
              index = int2Float ind
              pointStep = 1 / (int2Float (length output))


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
          adsrRatio' = zipWith (\x len -> (x / len):: Float) [20,10,50,20] $ repeat $ int2Float 10

wave :: [Pulse]
wave =
  concat
    [ zipWith3 (\x y z -> x + y + z) (note (-2) 5) (note 2 5) (note 5 5) ]

hehehe :: [Pulse]
hehehe = concat [ note 0 0.25
                , note 0 0.25
                , note 12 0.5
                , note 7 (0.5 + 0.25)
                , note 6 0.5
                , note 5 0.5
                , note 3 0.5
                , note 0 0.25
                , note 3 0.25
                , note 5 0.25
                ]

save :: FilePath -> IO ()
save filePath = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE hehehe

play :: IO ()
play = do
  save outputFilePath
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

main :: IO ()
main = save outputFilePath
