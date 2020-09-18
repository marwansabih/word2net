module Memory where

import           Control.Monad
import           Data.HashMap        as M
import           Data.List.Split
import           Data.Vector.Mutable as VM
import           System.Directory
import           System.IO
import           WordSet

saveWordSet :: WordSet -> IO ()
saveWordSet (file', text, wmap, dist, vs, us) = do
   let file = "save" ++ ( Prelude.drop (Prelude.length "texts") file')
   createDirectoryIfMissing True file
   saveMap (file ++ "/map" ) wmap
   saveDist (file ++ "/dist") dist
   saveVector (file ++ "/vs") vs
   saveVector (file ++ "/us") us

saveMap :: String -> HashMap String Int -> IO ()
saveMap file wmap = do
   let ks = keys wmap
   let es = elems wmap
   writeFile file ""
   withFile file AppendMode $ \handle -> do
      mapM_ (\(key, ele) -> hPutStr handle $ key ++" " ++ show ele ++ "\n") $ zip ks es

saveDist :: String -> [(String,Double)] -> IO ()
saveDist file dist = do
   writeFile file ""
   withFile file AppendMode $ \handle -> do
      mapM_ (\(key,ele) -> hPutStr handle $ key ++" " ++ show ele ++ "\n") dist

saveVector :: String -> WordVectors -> IO ()
saveVector file vec = do
   writeFile file ""
   withFile file AppendMode $ \handle -> do
      for [0.. (VM.length vec -1)] $ \idx -> do
         (key,v) <- VM.read vec idx
         hPutStr handle $ ( key ++ " " )
         for [0.. (VM.length v -1)] $ \idx2 -> do
            x <- VM.read v idx2
            hPutStr handle $ ( show x ++ " ")
         hPutStr handle $ "\n"

loadWordSet :: String -> IO WordSet
loadWordSet file = do
   rawText <- readFile file
   text <- toWords rawText
   let file' = "save" ++ ( Prelude.drop (Prelude.length "texts") file)
   wmap <- loadMap (file' ++ "/map")
   dist <- loadDist (file' ++ "/dist")
   vs <- loadVec (file' ++ "/vs")
   us <- loadVec (file' ++ "/us")
   return (file, text, wmap, dist, vs, us)

loadMap :: String -> IO (HashMap String Int)
loadMap file = do
   vs <- lines <$> readFile file
   let xs = Prelude.map ( (\[a,b] -> (a, Prelude.read b :: Int)) . (splitOn  " ")) vs
   return $ M.fromList xs

loadDist :: String -> IO [(String,Double)]
loadDist file  = do
   vs <- lines <$> readFile file
   return $ Prelude.map ( (\[a,b] -> (a, Prelude.read b :: Double)) . (splitOn  " ")) vs

loadVec :: String -> IO WordVectors
loadVec file = do
   vs <- lines <$> readFile file
   let pairs = Prelude.map ((\xs -> (Prelude.head xs,Prelude.tail xs)) . (splitOn " ") ) vs
   wvec <- VM.replicate 1 (0.0::Double)
   vec <- VM.replicate (Prelude.length pairs) (" ",wvec)
   for (zip [0..] pairs) $ \(idx, (word,values)) -> do
      v <- VM.replicate (Prelude.length values -1) (0.0::Double)
      for (zip [0..] (Prelude.init values)) $ \(idx2,value) -> do
         VM.write v idx2 (Prelude.read value :: Double)
      VM.write vec idx (word,v)
   return vec
