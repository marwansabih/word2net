module Main where

import           GHC.IO.Encoding
import           GradientDescent
import           Lib
import           Memory
import           Runner
import           System.Directory
import           WordSet

config = Config { file = "texts/HP.txt"
                             , vecLength   = 100
                             , window  = 10
                             , sampleSize  = 10
                             , learningRate = 0.0001
                           }

main :: IO ()
main = do
  setLocaleEncoding utf8
  --set <- generateWordSet "texts/HP.txt" 100
  --saveWordSet set
  set <- run 10 config
  print "Who is closest to Harry?"
  closest 50 "Harry" set >>= print
  print "Who is closest to Ron"
  closest 50 "Ron" set >>= print
