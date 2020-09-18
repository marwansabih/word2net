module Runner where
import           GradientDescent
import           Memory
import           System.Directory
import           WordSet

data Config = Config { file                        :: String
                                    , vecLength    :: Int
                                    , window       :: Int
                                    , sampleSize   :: Int
                                    , learningRate :: Double
                                    }

run :: Int -> Config -> IO WordSet
run epochs config = do
  let f = file config
  let f' =  "save/" ++ (drop (length "texts/") f)
  exist <-  elem (drop (length "save/") f') <$> listDirectory "save/"
  let vl = vecLength config
  let ws = window config
  let sz = sampleSize config
  let lr = learningRate config
  set <- if exist
    then loadWordSet f
    else generateWordSet f vl
  train epochs ws sz lr set

train :: Int -> Int -> Int -> Double -> WordSet -> IO WordSet
train 0 ws sz lr set = return set
train n ws sz lr set = do
  set <- trainText set ws sz lr
  print "Start Saving"
  saveWordSet set
  print $ "Epoch " ++ (show n)
  train (n-1) ws sz lr set
