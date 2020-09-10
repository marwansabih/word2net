module WordSet where
import           Control.Monad       (forM_)
import           Data.HashMap.Strict as M
import           Data.List
import           Data.Random.Normal
import           Data.Vector.Mutable as VM
import           GHC.Prim
import           System.Directory
import           System.IO

type WordVector = (String, MVector RealWorld Double)
type WordVectors = MVector RealWorld WordVector
type WordSet = (MVector RealWorld String, HashMap String Int , WordVectors, WordVectors)


for = Control.Monad.forM_
rawText = readFile "test.txt"

generateWordSet :: String -> Int -> IO WordSet
generateWordSet file size = do
  rawText <- readFile file
  text <- toWords rawText
  let ws = sort  $ toSet $ toWords' rawText
  let wmap = generateMap ws
  vs <- genWordVectors ws size
  us <- genWordVectors ws size
  return (text, wmap, vs, us)

displayWordSet :: WordSet -> IO ()
displayWordSet (_,_,vs,_) = do
  for [0..(VM.length vs -1)] $ \idx -> do
    (word, vec) <- VM.read vs idx
    let space = Prelude.concat $ Prelude.replicate (50 - Prelude.length word) " "
    putStr $ word ++ space
    for [0..(VM.length vec -1)] $ \idx -> do
      v <- VM.read vec idx
      putStr $ Data.List.take 4 (show v) ++ " "
    putStr "\n"

generateMap :: [String] -> HashMap String Int
generateMap ws = M.fromList pairs
  where
    pairs = zip ws [0..]

findEntry :: String -> HashMap String Int ->WordVectors -> IO WordVector
findEntry word wmap ws = do
    let Just idx = M.lookup word wmap
    VM.read ws idx


toWords :: String -> IO (MVector RealWorld String)
toWords text = do
  let ws = toWords' text
  vec <- VM.replicate (Prelude.length ws)  " "
  for (zip [(0::Int)..] ws) $ \(idx,word) -> do
    VM.write vec idx word
  return vec


toWords' :: String -> [String]
toWords' text = Prelude.map ( \word ->  foldl (\x y -> Data.List.delete y x)  word ['.', ',', ':', '!', '?'] ) rawWords
  where
    rawWords = words text

genWordVectors :: [String] -> Int -> IO WordVectors
genWordVectors ws size = do
  entry <- VM.replicate 1 0.0
  vector <- VM.replicate (Prelude.length ws) (" ", entry)
  for (zip [0..] ws) $ \(idx,word) -> do
    entries <- genWordVec size
    write vector idx (word,entries)
  return vector

toSet :: [String] -> [String]
toSet []     = []
toSet (x:xs) = x : (toSet xs')
  where
    xs' = Prelude.filter (/= x) xs

testWords :: IO (MVector RealWorld String)
testWords = rawText >>= toWords

printWords :: MVector RealWorld String -> IO ()
printWords ws = do
  for [0..(VM.length ws -1)] $ \idx -> do
    word <- VM.read ws idx
    print $ word ++ " "

genWordVec :: Int -> IO (MVector RealWorld Double)
genWordVec size = do
  samples <- Data.List.take size <$> normalsIO
  vec <- VM.replicate size 0.0
  for (zip [0..] samples) $ \(idx,sample) -> do
    VM.write vec idx sample
  return vec

test :: IO ()
test = do
  hSetEncoding stdout utf8
  file <- rawText
  print file
  print $ toWords' file
  print $ sort $ toSet $ toWords' file
  testWords >>= printWords
  set <- generateWordSet "earth.txt" 10
  displayWordSet set
