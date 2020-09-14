module WordSet where
import           Control.Monad       (forM_, replicateM)
import           Data.HashMap        as M
import           Data.List
--import           Data.Random.Normal
import           Data.Vector.Mutable as VM
import           GHC.Prim
import           System.Directory
import           System.IO
import           System.Random

type WordVector = (String, MVector RealWorld Double)
type WordVectors = MVector RealWorld WordVector
type WordSet = (String, MVector RealWorld String, HashMap String Int , [(String,Double)], WordVectors, WordVectors)


for = Control.Monad.forM_
rawText = readFile "test.txt"

generateWordSet :: String -> Int -> IO WordSet
generateWordSet file size = do
  rawText <- readFile file
  text <- toWords rawText
  let ws = sort  $ toSet $ toWords' rawText
  let dist  = unigramDist ws
  let wmap = generateMap ws
  vs <- genWordVectors ws size
  us <- genWordVectors ws size
  return (file, text, wmap, dist, vs, us)

drawNFromDist :: Int -> String -> [(String,Double)] -> IO [String]
drawNFromDist  n wordOut dict = drawNFromDist' n wordOut dict []

drawNFromDist' :: Int -> String -> [(String,Double)] -> [String] -> IO [String]
drawNFromDist' 0 _ _ found = return found
drawNFromDist' n wordOut dict found = do
  draw <- drawFromDist dict
  if (draw `elem` found || draw == wordOut )
    then drawNFromDist' n wordOut dict found
    else drawNFromDist' (n-1) wordOut dict (draw:found)

drawFromDist :: [(String,Double)] -> IO String
drawFromDist dist = do
  let (_,maxv) = last dist
  v <- randomRIO (0.0,maxv)
  let find v ((w,x):xs) = if x > v then w else find v xs
  return $ find v dist

unigramDist :: [String] -> [(String,Double)]
unigramDist ws = unigramDist' ws []

unigramDist' :: [String] -> [(String,Double)] -> [(String,Double)]
unigramDist' [] found     = found
unigramDist' (x:xs) [] = unigramDist' xs' [(x,nr)]
  where
    nr' = Prelude.length $ Prelude.filter (==x) xs
    nr = (fromIntegral nr' + 1.0)**(3/4)
    xs' = Prelude.filter (/=x) xs
unigramDist' (x:xs) ys = unigramDist' xs' (ys ++ [(x,nr)])
  where
    (_,pr)= last ys
    nr' = Prelude.length $ Prelude.filter (==x) xs
    nr = (fromIntegral nr' + 1)**(3/4) + pr
    xs' = Prelude.filter (/=x) xs

displayWordSet :: WordSet -> IO ()
displayWordSet (_,_,_,_,vs,_) = do
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
  samples <- Control.Monad.replicateM size $ randomRIO (-0.01::Double,0.01) -- Data.List.take size <$> normalsIO
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
