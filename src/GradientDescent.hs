import           Data.Vector.Mutable as VM
import           GHC.Prim
import           WordSet

trainText :: String -> Int -> Int -> Double -> IO WordSet
trainText file vecLength window s = do
   (text,wmap,wcs,wos) <-generateWordSet file vecLength
   for [window..(VM.length text - window -1)] $ \idx -> do
      print $ (fromIntegral idx) / (fromIntegral (VM.length text - 2* window) )
      wordCenter <-VM.read text idx
      for ([(idx-window)..(idx-1)] ++ [(idx+1)..(idx+window)]) $ \idx' -> do
         wordOut <- VM.read text idx'
         trainWords wmap wordOut wordCenter wcs wos s
   return (text,wmap,wcs,wos)

trainWords wmap wordOut wordCenter wcs wos s = do
   (_,v) <- findEntry wordCenter wmap wcs
   (_,u) <- findEntry wordOut wmap wos
   denominator <- softmaxDenominator wos v
   gradv <- gradV denominator wos u v
   gradv <- scalarMult s gradv
   addMV_ u gradv
   for [0..(VM.length wos -1)] $ \idx -> do
      (word, u) <- VM.read wos idx
      gradu <-gradU (word == wordOut) denominator wos u v
      gradu <- scalarMult s gradu
      addMV_ u gradu

scalarProduct :: MVector RealWorld Double -> MVector RealWorld Double -> IO Double
scalarProduct xs ys = do
   result <- VM.replicate 1 0.0
   for [0.. (VM.length xs -1)] $ \idx -> do
      x <- VM.read xs idx
      y <- VM.read ys idx
      v <-VM.read result 0
      VM.write result 0 (v+x*y)
   v <- VM.read result 0
   return v

softmax :: WordVectors -> MVector RealWorld Double -> MVector RealWorld Double -> IO Double
softmax ws x y = do
   numerator <- scalarProduct x y
   result <- VM.replicate 1 0.0
   for [0..(VM.length ws -1)] $ \idx -> do
      (_,z)<- VM.read ws idx
      value <- scalarProduct z x
      v <- VM.read result 0
      VM.write result 0 (v+(exp value ))
   v <- VM.read result 0
   return $ (exp v) / numerator

zipwithMV_ :: (Double -> Double -> Double) -> MVector RealWorld Double -> MVector RealWorld Double -> IO ()
zipwithMV_ f u v = do
   for [0..(VM.length u -1)] $ \idx -> do
      x <- VM.read u idx
      y <- VM.read v idx
      VM.write u idx (f x y)

addMV_ u v = zipwithMV_ (+) u v

zipwithMV :: (Double -> Double -> Double) -> MVector RealWorld Double -> MVector RealWorld Double -> IO (MVector RealWorld Double)
zipwithMV f u v = do
   vec <- VM.replicate (VM.length u) 0.0
   for [0..(VM.length vec -1)] $ \idx -> do
      x <- VM.read u idx
      y <- VM.read v idx
      VM.write vec idx (f x y)
   return vec

addMV u v = zipwithMV (+) u v

subMV u v = zipwithMV (-) u v

scalarMult :: Double -> MVector RealWorld Double -> IO (MVector RealWorld Double)
scalarMult a xs = do
   vec <- VM.replicate (VM.length xs) 0.0
   for [0..(VM.length xs -1)] $ \idx -> do
      v <- VM.read xs idx
      VM.write vec idx (a*v)
   return vec

softmaxDenominator :: WordVectors -> MVector RealWorld Double -> IO Double
softmaxDenominator ws v = do
   result <- VM.replicate 1 0.0
   for [0..(VM.length ws -1)] $ \idx -> do
      (_,z)<- VM.read ws idx
      value <- scalarProduct z v
      v <- VM.read result 0
      VM.write result 0 (v+(exp value ))
   v <- VM.read result 0
   return v

gradV :: Double -> WordVectors -> MVector RealWorld Double -> MVector RealWorld Double -> IO (MVector RealWorld Double)
gradV denominator ws u v  = do
   vec <- VM.replicate (VM.length u) 0.0
   for [0..(VM.length ws -1)] $ \idx -> do
      (_,ux) <- VM.read ws idx
      nominator <- exp <$> scalarProduct ux v
      ux' <- scalarMult (nominator/denominator) ux
      addMV_ vec ux'
   subMV vec u

gradU :: Bool -> Double ->WordVectors -> MVector RealWorld Double -> MVector RealWorld Double -> IO (MVector RealWorld Double)
gradU same denominator ws u v = do
   vec <- VM.replicate (VM.length u) 0.0
   nominator <- exp <$> scalarProduct u v
   let p = nominator/denominator
   if same then scalarMult (p-1) v else scalarMult p v

testTraining :: IO ()
testTraining = do
   set <- trainText "earth.txt" 5 2 0.1
   displayWordSet set

main :: IO ()
main = testTraining
