module Main where

import           GradientDescent
import           Lib
import           Runner
import           System.Directory
import           WordSet

config = Config { file = "texts/HP.txt"
                             , vecLength   = 100
                             , window  = 10
                             , sampleSize  = 10
                             , learningRate = 0.001
                           }

main :: IO ()
main = do
  set <- run 10 config
  print "Who is closest to Harry?"
  closest "Harry" set >>= print
  print "Who is closest to Ron"
  closest "Ron" set >>= print
