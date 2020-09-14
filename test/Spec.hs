import           Control.Monad
import           Data.HashMap        as M
import           Data.Vector.Mutable as VM
import           GradientDescent
import           Memory
import           Test.Hspec
import           Test.QuickCheck
import           WordSet

testWCS :: IO WordVectors
testWCS = do
  let numbers = ["one","two","three","four","five","six","seven","eight","nine","ten"]
  vec <-VM.replicate 10 0.0
  wcs <- VM.replicate 10 (" ",vec)
  for (zip [1..] numbers) $ \(idx,word) -> do
    vec <- VM.replicate 10  1.0
    VM.write wcs (idx-1) (word, vec )
  return wcs

main :: IO ()
main = hspec $ do
  it "Should be a the set from 1..20" $ do
    (_,set,wmap,_,wcs,wos) <- generateWordSet "texts/test.txt" 10
    print wmap
  it "genWordVec" $ do
    vec <- genWordVec 10
    for [0..9] $ \idx -> do
      d <- VM.read vec idx
      print d

  it "addMV_" $ do
    v1 <- VM.replicate 10 1.0
    v2 <- VM.replicate 10 2.0
    addMV_ v1 v2
    displayVector v1

  it "softmaxDenominator" $ do
    vec <- VM.replicate 10 1.0
    wcs <- testWCS
    d <- softmaxDenominator wcs vec
    shouldSatisfy d (\v -> (v - abs(10.0 * (exp (10-100))) < 0.0001))

  it "gradV" $ do
    v <- VM.replicate 10 1.0
    u <- VM.replicate 10 2.0
    wcs <- testWCS
    d <- softmaxDenominator wcs v
    vec <- gradV d wcs u v
    displayVector vec

  it "gradU" $ do
    v <- VM.replicate 10 1.0
    u <- VM.replicate 10 1.0
    wcs <- testWCS
    d <- softmaxDenominator wcs v
    vec <- gradU False d wcs u v
    displayVector vec
    vec <- gradU True d wcs u v
    displayVector vec

  it "trainTextClassic" $ do
    set <- generateWordSet "texts/test.txt" 5
    set <- trainTextClassic set 2 0.01
    c <- closest "six" set
    print c

  it "unigramDist" $ do
    let dist = unigramDist ["test","hallo","hi","test","hallo","hi","hallo","test","hi","hallo","hi","test"]
    print dist
    drawFromDist dist >>= print
    drawFromDist dist >>= print
    drawFromDist dist >>= print
    drawFromDist dist >>= print
    drawNFromDist 2 "hallo" dist >>= print

  it "gradUNegSample" $ do
    v <- VM.replicate 10 1.0
    u <- VM.replicate 10 1.0
    vec <- gradUNegSample True u v
    v1 <- VM.read vec 0
    v1  `shouldBe` (-1)*(1/(exp 10 + 1))
    displayVector vec
    vec <- gradUNegSample False u v
    v1 <- VM.read vec 0
    v1  `shouldBe` (1/(exp (-10) + 1))
    displayVector vec

  it "gradVNegSample" $ do
    v <- VM.replicate 10 1.0
    u <- VM.replicate 10 1.0
    us <- VM.replicate 10 u
    vec <- gradVNegSample u us v
    v1 <- VM.read vec 0
    v1 `shouldBe`  ((-1)*(1/(exp 10 + 1))+ 10 * (1/(exp (-10) + 1)))
    displayVector vec

  it "readWordVectorsByName" $ do
    let wmap = M.fromList $ zip ["one","two","three","four","five","six","seven","eight","nine","ten"] [0..]
    wos <- testWCS
    vs <- readWordVectorsByName ["three","four","seven"] wmap  wos
    for [0..(VM.length vs -1)] $ \idx -> do
      v <- VM.read vs idx
      displayVector v

  it "trainText" $ do
    set <- generateWordSet "texts/test.txt" 100
    set <- trainText set 2 10 0.001
    c <- closest "six" set
    print c

  it "loadWordSet and saveWordSet" $ do
    (file,text,wmap,dist,us,vs) <- generateWordSet "texts/test.txt" 10
    saveWordSet (file,text,wmap,dist,us,vs)
    (file',text',wmap',dist',us',vs') <- loadWordSet "texts/test.txt"
    file `shouldBe` file'
    for [0..(VM.length text -1)] $ \idx -> do
      w <- VM.read text idx
      w' <- VM.read text' idx
      w `shouldBe` w'
    wmap `shouldBe` wmap'
    dist `shouldBe` dist'
    for [0..(VM.length us -1)] $ \idx -> do
      (w,v) <- VM.read us idx
      (w',v') <- VM.read us' idx
      (w2,v2) <- VM.read vs idx
      (w2',v2') <- VM.read vs' idx
      w `shouldBe` w'
      w2 `shouldBe` w2'
      for [0.. (VM.length v -1)] $ \idx2 -> do
        i <- VM.read v idx2
        i' <- VM.read v' idx2
        j <- VM.read v2 idx2
        j' <- VM.read v2' idx2
        i `shouldBe` i'
        j  `shouldBe` j'
    (_,v) <- VM.read us 0
    (_,u) <- VM.read us' 0
    VM.length v `shouldBe` VM.length u

  it "training_after_loadWordSet" $ do
    set <- generateWordSet "texts/test.txt" 10
    saveWordSet set
    set <- loadWordSet "texts/test.txt"
    set <- trainText set 2 10 0.001
    c <- closest "six" set
    print c
