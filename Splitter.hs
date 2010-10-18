{-# LANGUAGE DeriveDataTypeable #-}

module Main where
import Data.Array
import Data.List
import System.IO
import System.CPUTime
import System.FilePath
import System
import Control.Monad
import System.Console.CmdArgs

data Item a = Item {
   item :: a,       -- acid,...
   weight :: Int    -- weight of the item
} deriving (Eq, Show, Ord)

data Stream a = Stream {
   price :: Int,    -- price of the stream
   low :: a,        -- first item of the range
   high :: a        -- last item of the range
} deriving (Eq, Show, Ord)

data Cell a = Cell {
   cost :: a,       -- cost of the partition
   ind :: Int       -- position of the partition in items
} deriving (Eq, Show, Ord)

data Splitter = Splitter {
  file_ :: FilePath,
  numstreams_ :: Int,
  greedy_ :: Bool,
  naive_ :: Bool,
  optimal_ :: Bool,
  threshold_ :: Int
} deriving (Show, Data, Typeable)


splitter = cmdArgsMode $ Splitter
    {file_ = def &= typFile &= help "Input file name. Format:  <acid> <weight> <eol>"
    ,numstreams_ = def &= opt "1" &= name "s" &= typ "Int" &= help "Number of streams"
    ,greedy_ = def &= name "g" &= help "Greedy algorithm"
    ,naive_ = def &= name "n" &= help "Naive algorithm"
    ,optimal_ = def &= name "o" &= help "Optimal algorithm"
    ,threshold_ = def &= opt "1" &= help "Threshold to combine the consequtive weights"
     } &=
    program "Splitter" &=
    summary "Splitter 0.1" &=
    help "Calculate multistream counters for HiPerformance drivers" &=
    details ["1The v17.14 implementation of the splitters:",
                "  Naive algoritm:   inflogen, tranagg, retcalc, updret, recon",
                "                    composite rec, dispstore, opxreclass, grpmember",
                "  Greedy algorithm: opflogen, opsubdrv, bnchrecalc"]

main :: IO ()
main = do
  cnf <- cmdArgsRun splitter
  eval cnf  

eval :: Splitter -> IO ()
eval cnf = do
  inh <- openFile (file_ cnf) ReadMode
  rows <- hGetContents inh
  let items = map mkItem $ filter ((== 2).length) $ map words $ lines rows
  let numStreams = numstreams_ cnf
  
  when (optimal_ cnf) $ do
    let threshold = threshold_ cnf
    let astreams = (unshrink . streams . best numStreams . shrink (merge threshold)) items
    display "Approximation Best" astreams
    
  when (greedy_ cnf) $ do   
    let gstreams = streams $ greedy numStreams items (sum . map weight)
    display "Greedy" gstreams
   
  when (naive_ cnf) $ do
    let nstreams = streams $ greedy numStreams items length
    display "Naive" nstreams
     
  hClose inh  

 
merge a x y = weight x <= a && weight y <= a

mkItem xs = Item (getInt $ head xs) (getInt $ (head . tail) xs) where
  getInt x = read x :: Int
  
streams xss =  map mkStream xss where
   mkStream xs = Stream (sum $ map weight xs) (item $ head xs) (item $ last xs)
      
display title streams = do
   putStrLn $ "\n     " ++ title
   t1 <- getCPUTime
   mapM_ print streams
   t2 <- getCPUTime
   print $ "   Costing = " ++ show (foldr1 max streams)
   print $ "   Total price = " ++ show (sum $ map price streams)
   print $ "   Standard deviation = " ++ show (floor $ stddev $ map (fromIntegral . price) streams)
   print $ "   Time to execute = " ++ show (div (t2-t1) 1000000000)

best :: Int -> [Item a] -> [[Item a]]
best size items = slices dividers items 
   where
      dividers | noItems <= size = [0..noItems-1]
               | otherwise = nub $ reverse $ cells size $ valOf noItems size
      
      cells 1 cell = [0]
      cells k cell = ind cell : cells (k-1) (valOf (ind cell) (k-1))  

      table = array ((1,1), (noItems, size)) 
              [
                ((m,n), cell m n) |
                m <- [1..noItems],
                n <- [1..size]
              ]

      valOf m n 
       | m == 1 = Cell (weight $ itemsArr ! 1) 1
       | n == 1 = Cell (prefSums ! m) 1
       | otherwise = table ! (m,n)

      cell m n = foldr1 min $ map maxCost [1..m] where
        maxCost x = Cell (max (curCost x) $ newCost x) x
        curCost x = cost $ valOf x (n-1)
        newCost x = prefSums ! m - prefSums ! x
        
      noItems = length items
      itemsArr = listArray (1, noItems) items
      prefSums = listArray (1, noItems) $ scanl1 (+) (map weight items)
      
      slices xs items =  map slice ls where
        ls = zip xs (tail (xs ++ [length items]))
        slice (lo, hi) = take (hi-lo) $ drop lo items
 
shrink :: (Item a -> Item a -> Bool) -> [Item a] -> [Item (a,a)]
shrink thr items = map mkItem' $ groupBy thr items where
  mkItem' xs = Item (lo xs, hi xs) $ sum $ map weight xs
  lo = item . head
  hi = item . last

unshrink :: [Stream (a,a)] -> [Stream a]
unshrink = map (\(Stream cost lo hi) -> Stream cost (fst lo) (snd hi))

-- Greedy emulation
greedy :: Int -> [Item a] -> ([Item a] -> Int) -> [[Item a]]
greedy n xs f = go n xs f where
  go _ [] _ = []
  go 1 ys _ = [ys] 
  go n ys f = 
   let cands = dropWhile (\xs -> avg > f xs) ((tail . inits) ys)
       chunk = if null cands then ys else head cands 
       rest = drop (length chunk) ys in
         chunk : go (n-1) rest f
  avg = f xs `div` n
  
-- Statistics
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

var :: [Double] -> Double
var xs = sum (map f xs) / (fromIntegral (length xs) - 1) where
  f x = (x - mean xs) ^ 2

stddev :: [Double] -> Double
stddev = sqrt . var