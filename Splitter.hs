{-# LANGUAGE DeriveDataTypeable   #-}

-- |
-- Module      :  Main
-- Copyright   :  (c) Vitaliy Rkavishnikov, 2011
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Balance the work items accross processors to minimize the total elapsed time.
-- The Splitter supports the next partitioning algorithms:
--    a) naive  - partition the work items into equal ranges
--    b) greedy - partition the work items into ranges based on the average cost
--    c) best   - linear partition the work items using dynamic programming.  
--                The large size of the work items space can be decreased by
--                combining the consecutive items based on the threshold parameter.

module Main where
import Data.Array
import Data.List
import System.IO
import System.CPUTime
import System.FilePath
import System
import Control.Monad
import System.Console.CmdArgs
import Test.QuickCheck hiding (ranges, shrink)

-- / Representation of the work item
data Item a = Item {
   item :: a,       -- item id
   weight :: Double -- weight of the item
} deriving (Eq, Show, Ord)

-- / Range of work items
data Range a = Range {
   price :: Double, -- cost of the range
   low :: a,        -- first item of the range
   high :: a        -- last item of the range
} deriving (Eq, Show, Ord)

-- / Partition algorithm type
type Alg a = Int -> [Item a] -> [[Item a]]

-- / The table cell to store the computed partitions
data Cell = Cell {
   cost :: Double,  -- cost of the partition
   ind  :: Int      -- partition index in the work items
} deriving (Eq, Show, Ord)

-- / Splitter configuration parameters
data Splitter = Splitter {
  file_ :: FilePath,
  numranges_ :: Int,
  greedy_ :: Bool,
  naive_ :: Bool,
  optimal_ :: Bool,
  threshold_ :: Double
} deriving (Show, Data, Typeable)

splitter = cmdArgsMode $ Splitter
    {file_ = def &= typFile &= help "Input file name. Format:  <item> <weight> <eol>"
    ,numranges_ = def  &= name "s" &= typ "Int" &= help "Number of ranges"
    ,greedy_ = def &= name "g" &= help "Greedy algorithm"
    ,naive_ = def &= name "n" &= help "Naive algorithm"
    ,optimal_ = def &= name "o" &= help "Optimal algorithm"
    ,threshold_ = def &=  help "Threshold to combine the consequtive weights"
     } &=
    program "Splitter" &=
    summary "Splitter 0.1" &=
    help "Partition the list of items into number of ranges for the parallel execution" &=
    details []

main :: IO ()
main = do
  cnf <- cmdArgsRun splitter
  eval cnf  

eval :: Splitter -> IO ()
eval cnf = do
  inh <- openFile (file_ cnf) ReadMode
  rows <- hGetContents inh
  let items = map mkItem $ filter ((== 2).length) $ map words $ lines rows
  let numRanges = let n = numranges_ cnf in 
                  if n == 0 then 1 else n 
  
  when (optimal_ cnf) $ do
    let threshold = threshold_ cnf
    let ranges = unshrink $ optimal numRanges (shrink (merge threshold) items)
    display "Approximation Best" ranges
    
  when (greedy_ cnf) $ do   
    let ranges = greedy numRanges items
    display "Greedy" ranges
   
  when (naive_ cnf) $ do
    let ranges = naive numRanges items
    display "Naive" ranges
     
  hClose inh  

-- / Optimal partitioning algorithm 
optimal :: Int -> [Item a] -> [Range a] 
optimal = partitions best

-- / Partitioning is based on the number of the work items
naive :: Int -> [Item a] -> [Range a] 
naive   = partitions (greedyBy (fromIntegral . length))

-- / Partitioning is based on the average cost of the range
greedy :: Int -> [Item a] -> [Range a] 
greedy = partitions (greedyBy (sum . map weight))

-- / Generic partitioning
partitions :: Alg a -> Int -> [Item a] -> [Range a]
partitions alg n = ranges . alg n

-- / Combine the consecutive items to decrease the space of the input
merge :: Double -> Item a -> Item a -> Bool
merge i x y = weight x <= i && weight y <= i

-- / Parse items
mkItem :: [String] -> Item String
mkItem xs = Item (read $ head xs) (read $ (head . tail) xs) where

-- / Create ranges
ranges :: [[Item a]] -> [Range a]
ranges xss =  map mkRange xss where
   mkRange xs = Range (sum $ map weight xs) (item $ head xs) (item $ last xs)

totalCost :: [Range a] -> Double
totalCost = sum . map price

maxCost :: Ord a => [Range a] -> Double
maxCost [] = 0.0
maxCost xs = price $ foldr1 max xs
      
-- / Display the results of the Splitter execution 
display :: (Show a, Ord a) => String -> [Range a] -> IO ()
display title ranges = do
   putStrLn $ "\n     " ++ title
   t1 <- getCPUTime
   mapM_ print ranges
   t2 <- getCPUTime
   print $ "   Max cost = " ++ show (foldr1 max ranges)
   print $ "   Total cost = " ++ show (totalCost ranges)
   print $ "   Time to execute = " ++ show (div (t2-t1) 1000000000)

-- / Partition items to minimize the maximum cost over all ranges
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

-- / Grouping the small items
shrink :: (Item a -> Item a -> Bool) -> [Item a] -> [Item (a,a)]
shrink thr items = map mkItem' $ groupBy thr items where
  mkItem' xs = Item (lo xs, hi xs) $ sum $ map weight xs
  lo = item . head
  hi = item . last

-- / Ungrouping the items
unshrink :: [Range (a,a)] -> [Range a]
unshrink = map (\(Range cost lo hi) -> Range cost (fst lo) (snd hi))

-- / Partition the items based on the greedy algoritm
greedyBy :: ([Item a] -> Double) -> Int -> [Item a] -> [[Item a]]
greedyBy f n xs = go n xs f where
  go _ [] _ = []
  go 1 ys _ = [ys] 
  go n ys f = 
   let cands = dropWhile (\xs -> avg > f xs) ((tail . inits) ys)
       chunk = if null cands then ys else head cands 
       rest = drop (length chunk) ys in
         chunk : go (n-1) rest f
  avg = f xs / fromIntegral n

-- / Testing
data Split = Split {
   chunks :: Int,
   items :: [Item String]
} deriving (Show)

instance Arbitrary a => Arbitrary (Item a) where
   arbitrary = do 
      i <- arbitrary 
      w <- choose (0.0, 1000000.0)
      return $ Item i w

instance Arbitrary Split where
   arbitrary = do 
      n <- choose (1,25) :: Gen Int
      is <- arbitrary
      return $ Split n is

qcArgs :: Args 
qcArgs = Args {
  replay = Nothing,
  maxSuccess = 200,
  maxDiscard = 500,
  maxSize = 200
}

-- / Ensure that the sum of the items weights equals to 
-- the total ranges costs 
prop_totalCost s = 
   let (n,xs) = (chunks s, items s)
       splitters = [optimal n xs, naive n xs, greedy n xs]
       totalCosts = map (floor . totalCost) splitters
       itemsCost = floor $ sum $ map weight xs 
   in all (== itemsCost) totalCosts

-- / The optimal algorithm has to produce the lowest partition cost
prop_bestCost s = 
   let (n,xs) = (chunks s, items s)
       partitionCost = floor . maxCost 
       bestCost = partitionCost (optimal n xs)
   in bestCost <= partitionCost (greedy n xs) &&
      bestCost <= partitionCost (naive n xs)

-- / Ensure that the real number of ranges no more than required
prop_numRanges = forAll (arbitrary :: Gen Split) $ \s ->
  let (n,xs) = (chunks s, items s) 
  in length (optimal n xs) <= n &&
     length (greedy n xs) <= n &&
     length (naive n xs) <= n

-- / Ensure that the splitting dividers are ordered as working items
prop_ordered s = 
  let (n,xs) = (chunks s, items s)
      splitters = [optimal n xs, naive n xs, greedy n xs]
      divs = map (foldr dividers []) splitters
      dividers r xs = if low r == high r then low r : xs
                       else low r : (high r : xs)
  in all (ordered (map item xs)) divs

-- / Reverse working items preserves the optimal cost
prop_reverse s =
  let (n,xs) = (chunks s, items s)
      partitionCost = floor . maxCost
  in partitionCost (optimal n xs) == partitionCost (optimal n (reverse xs))

-- / Ensure that the ranges prices equal to the sum of weight corresponding
-- work items
prop_rangeCost s =
  let (n,xs) = (chunks s, items s)
  in eqCost (optimal n xs) xs
--      splitters = [optimal n xs] --, naive n xs, greedy n xs]
--  in and [eqCost rs xs | rs <- splitters] 

-- / Helpers
ordered :: [String] -> [String] -> Bool
ordered [] [] = True
ordered (x:xs) (y:ys) 
   | x == y = ordered xs ys
   | otherwise = ordered xs (y:ys)
ordered _ _ = False

eqCost :: [Range String] -> [Item String] -> Bool
eqCost [] [] = True
eqCost (Range p l h:xs) ys =
        let (ks,zs) = span (\(Item i _) -> i /= h) ys
            (ks',zs') = (ks ++ [head zs], tail zs)
        in and [(item . head) ks' == l
               ,(item . last) ks' == h
               ,floor (sum (map weight ks')) == floor p 
               ,eqCost xs zs'
               ] 

-- / Tests
testNumRanges = quickCheck prop_numRanges 
testOrdered   = quickCheck prop_ordered
testReverse   = quickCheck prop_reverse
testTotalCost = quickCheck (\s -> collect (null $ items s) (prop_totalCost s))
testBestCost  = quickCheckWith qcArgs prop_bestCost
testRangeCost = quickCheck prop_rangeCost
