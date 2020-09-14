module GradientDescent where
import           Data.HashMap        as M
import           Data.Time
import           Data.Vector.Mutable as VM
import           GHC.Prim
import           WordSet

trainText :: WordSet -> Int -> Int -> Double -> IO WordSet
trainText (file,text,wmap,dist,wcs,wos) window sampleSize s = do
   for [window..(VM.length text - window -1)] $ \idx -> do
      print $ (fromIntegral (idx-window)) / (fromIntegral (VM.length text - 2* window) )
      wordCenter <-VM.read text idx
      (_,v) <- findEntry wordCenter wmap wcs
      vec <- VM.replicate (VM.length v) 0.0
      store <- generateMatrixStore window sampleSize (VM.length v)
      for ([(idx-window)..(idx-1)] ++ [(idx+1)..(idx+window)]) $ \idx' -> do
         wordOut <- VM.read text idx'
         ws <- drawNFromDist sampleSize wordOut dist
         (_,uo) <- findEntry wordOut wmap wos
         us <- readWordVectorsByName ws wmap wos
         v' <- gradVNegSample uo us v
         let toIdx i = if ( i - (idx-window) > window) then i - (idx-window) -1 else i -  (idx-window)
         addGradUS (toIdx idx')  store us uo v
         addMV_ vec v'
      vec <- scalarMult s vec
      addMV_ v vec
      applyGradUSFromStore store s
      getCurrentTime >>= print
      displayVector v
   return (file,text,wmap,dist,wcs,wos)


trainTextClassic :: WordSet -> Int -> Double -> IO WordSet
trainTextClassic (file,text,wmap,dist,wcs,wos) window s = do
   for [window..(VM.length text - window -1)] $ \idx -> do
      print $ (fromIntegral idx) / (fromIntegral (VM.length text - 2* window) )
      wordCenter <-VM.read text idx
      (_,v) <- findEntry wordCenter wmap wcs
      vec <- VM.replicate (VM.length v) 0.0
      us <- generateMatrix (VM.length wcs) (VM.length v)
      for ([(idx-window)..(idx-1)] ++ [(idx+1)..(idx+window)]) $ \idx' -> do
         wordOut <- VM.read text idx'
         (v',us')<- trainWords wmap wordOut wordCenter wcs wos s
         addMV_ vec v'
         addMatrix us us'
      addMV_ v vec
      addMatrixToWS us wos
      displayVector v
   return (file,text,wmap,dist,wcs,wos)

readWordVectorsByName :: [String] -> HashMap String Int -> WordVectors  -> IO (MVector RealWorld (MVector RealWorld Double))
readWordVectorsByName ws wmap wos = do
   vec <- VM.replicate 1 0.0
   vs <-VM.replicate (Prelude.length ws) vec
   for [0..(VM.length vs -1)] $ \idx -> do
      (_,v) <- findEntry (ws !! idx) wmap wos
      VM.write vs idx v
   return vs

generateMatrix :: Int -> Int ->  IO (MVector RealWorld (MVector RealWorld Double))
generateMatrix m n = do
   vec <-VM.replicate n 0.0
   matrix <- VM.replicate m vec
   for [0..(m-1)] $ \idx -> do
      v <-VM.replicate n 0.0
      VM.write matrix idx v
   return matrix

type MatrixStore = MVector RealWorld (MVector RealWorld (MVector RealWorld Double, MVector RealWorld Double))

generateMatrixStore :: Int -> Int -> Int -> IO MatrixStore
generateMatrixStore window sampleSize vecLength = do
   vec <-VM.replicate vecLength 0.0
   matrix <- VM.replicate (sampleSize + 1)  (vec,vec)
   VM.replicate (2*window) matrix

addGradUS :: Int -> MatrixStore -> MVector RealWorld (MVector RealWorld Double) -> MVector RealWorld Double -> MVector RealWorld Double -> IO ()
addGradUS idx store us uo v = do
   matrix <- VM.read store idx
   gradu <- gradUNegSample True uo v
   VM.write matrix 0 (uo, gradu)
   for [1..VM.length us] $ \idx' -> do
      u <- VM.read us (idx' -1)
      gradu <- gradUNegSample False u v
      VM.write matrix idx' (u,gradu)

applyGradUSFromStore :: MatrixStore -> Double -> IO ()
applyGradUSFromStore store s = do
   for [0.. (VM.length store -1)] $ \idx -> do
      matrix <- VM.read store idx
      for [0.. (VM.length matrix -1)] $ \idx2 -> do
         (u,gradu) <- VM.read matrix idx2
         gradu <- scalarMult s gradu
         addMV_ u gradu

addMatrixToWS ::  MVector RealWorld (MVector RealWorld Double) -> WordVectors  ->  IO ()
addMatrixToWS  m1 wcs = do
   for [0..(VM.length m1 -1)] $ \idx -> do
      v1 <- VM.read m1 idx
      (_,v2) <- VM.read wcs idx
      for [0..(VM.length v1 -1)] $ \idx2 -> do
         x <- VM.read v1 idx2
         y <- VM.read v2 idx2
         VM.write v2 idx2 (x+y)


addMatrix ::  MVector RealWorld (MVector RealWorld Double) -> MVector RealWorld (MVector RealWorld Double) ->  IO ()
addMatrix m1 m2 = do
   for [0..(VM.length m1 -1)] $ \idx -> do
      v1 <- VM.read m1 idx
      v2 <- VM.read m2 idx
      for [0..(VM.length v1 -1)] $ \idx2 -> do
         x <- VM.read v1 idx2
         y <- VM.read v2 idx2
         VM.write v1 idx2 (x+y)

trainWords wmap wordOut wordCenter wcs wos s = do
   (_,v) <- findEntry wordCenter wmap wcs
   (_,u) <- findEntry wordOut wmap wos
   denominator <- softmaxDenominator wos v
   gradv <- gradV denominator wos u v
   gradv <- scalarMult s gradv
   gradus <- VM.replicate (VM.length wos) gradv
   for [0..(VM.length wos -1)] $ \idx -> do
      (word, u) <- VM.read wos idx
      gradu <-gradU (word == wordOut) denominator wos u v
      gradu <- scalarMult s gradu
      VM.write gradus idx gradu
   return (gradv, gradus)

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

subMV_ u v = zipwithMV_ (-) u v

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
      VM.write result 0 (v+(exp (value-100) ))
   v <- VM.read result 0
   return v

gradUNegSample :: Bool -> MVector RealWorld Double -> MVector RealWorld Double -> IO (MVector RealWorld Double)
gradUNegSample True u v = do
   sp <- scalarProduct  u v
   let sig = sigma ((-1.0)* sp) * (-1.0)
   scalarMult sig v
gradUNegSample False u v = do
   sp <- scalarProduct  u v
   scalarMult (sigma sp) v

gradVNegSample :: MVector RealWorld Double -> MVector RealWorld (MVector RealWorld Double ) -> MVector RealWorld Double -> IO (MVector RealWorld Double)
gradVNegSample uo us v = do
   sp <- scalarProduct  uo v
   let sig = sigma ((-1.0)* sp) * (-1.0)
   vec <-scalarMult sig uo
   for [0..(VM.length us -1)] $ \idx -> do
      u <- VM.read us idx
      sp <- scalarProduct  u v
      vec' <- scalarMult (sigma sp) u
      addMV_ vec vec'
   return vec

gradV :: Double -> WordVectors -> MVector RealWorld Double -> MVector RealWorld Double -> IO (MVector RealWorld Double)
gradV denominator ws u v  = do
   vec <- VM.replicate (VM.length u) 0.0
   for [0..(VM.length ws -1)] $ \idx -> do
      (_,ux) <- VM.read ws idx
      value <- scalarProduct ux v
      let nominator =  exp (value - 100 )
      ux' <- scalarMult (nominator/denominator) ux
      addMV_ vec ux'
   subMV vec u

gradU :: Bool -> Double ->WordVectors -> MVector RealWorld Double -> MVector RealWorld Double -> IO (MVector RealWorld Double)
gradU same denominator ws u v = do
   vec <- VM.replicate (VM.length u) 0.0
   value <- scalarProduct u v
   let nominator = exp (value - 100)
   let p = nominator/denominator
   if same then scalarMult (p-1) v else scalarMult p v

distance :: MVector RealWorld Double -> MVector RealWorld Double -> IO Double
distance v1 v2 = do
   dist <- VM.replicate 1 0.0
   for [0..(VM.length v1 -1)] $ \idx -> do
      x <- VM.read v1 idx
      y <- VM.read v2 idx
      v <- VM.read dist 0
      VM.write dist 0 (v + (x-y)^2)
   v <- VM.read dist 0
   return $ sqrt v

cosDistance v1 v2 = do
   v <- scalarProduct v1 v2
   a <- sqrt <$> scalarProduct v1 v1
   b <- sqrt <$> scalarProduct v2 v2
   return $ (1 - v/(a*b))

sigma :: Double -> Double
sigma x = 1/(exp (-x) + 1)

closest :: String -> WordSet -> IO String
closest word (_,_,wmap,_,wcs,_) = do
   (_,vec) <- findEntry word wmap wcs
   found <- VM.replicate 1 "NotFound"
   minDist <- VM.replicate 1 (100.0)
   for [0..(VM.length wcs -1)] $ \idx -> do
      (word',vec') <- VM.read wcs idx
      dist <-VM.read minDist 0
      dist' <- distance vec vec'
      if dist' < dist && word' /= word
         then do
          VM.write found 0 word'
          VM.write minDist 0 dist'
         else return ()
   word <-VM.read found 0
   return word

displayVector :: MVector RealWorld Double -> IO ()
displayVector vec = do
   for [0..VM.length vec -1] $ \idx -> do
      v <- VM.read vec idx
      putStr $ (show v) ++ " "
   putStrLn ""

testTraining :: IO ()
testTraining = do
   set <- generateWordSet "texts/HP.txt" 100
   set <- trainText set 5 10 0.001
   print "Who is closest to Harry?"
   closest "Harry" set >>= print
   print "Who is closest to Ron"
   closest "Ron" set >>= print
