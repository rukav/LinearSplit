{-# LANGUAGE DeriveDataTypeable   #-}

-- |
-- Module      :  Main
-- Copyright   :  (c) Vitaliy Rkavishnikov, 2011
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable

-- Balance the work items accross processors to minimize the total elapsed time.
--
-- For example, the next program invocation  will split the items in test1.txt on 
-- 5 ranges using greedy heuristics and linear partition algorithm.
-- $ Splitter -f test1.txt -n -o -g -t500 -s5


module Main where
import System.IO
import System.CPUTime (getCPUTime)
import Control.Monad (when)
import System.Console.CmdArgs
import Data.LinearSplit

type AccountId = Int
type NumRecords = Int
type Account = Item AccountId NumRecords

-- | Range of accounts
data Range = Range {
   price :: NumRecords,     -- cost of the range
   low :: AccountId,        -- first item of the range
   high :: AccountId        -- last item of the range
} deriving (Eq, Show, Ord)

-- / Splitter configuration parameters
data Splitter = Splitter {
  file_ :: FilePath,
  numranges_ :: Int,
  greedy_ :: Bool,
  trivial_ :: Bool,
  optimal_ :: Bool,
  threshold_ :: Int
} deriving (Show, Data, Typeable)

splitter = cmdArgsMode $ Splitter
    {file_ = def &= typFile &= help "Input file name. Format:  <account> <weight> <eol>"
    ,numranges_ = def  &= name "s" &= typ "Int" &= help "Number of ranges"
    ,greedy_ = def &= name "g" &= help "Greedy algorithm based on the average cost of a partition"
    ,trivial_ = def &= name "n" &= help "Greedy algorithm based on the average size of a partition"
    ,optimal_ = def &= name "o" &= help "Approximate linear partition algorithm"
    ,threshold_ = def &=  help "Threshold to combine the consequtive weights"
     } &=
    program "Splitter" &=
    summary "Splitter 0.1" &=
    help "Partition the list of accounts into number of ranges for the parallel execution" &=
    details []

-- | Create ranges
--ranges :: (Ord b, Num b) => [[Item a b]] -> [Range a b]
ranges xss =  map mkRange xss where
   mkRange xs = Range (sum $ map weight xs) (item $ head xs) (item $ last xs)

-- / Partitions algorithms
optimal = ltPartition

trivial n xs = gPartition fun n xs where
   fit = length xs `div` n 
   fun ys = fit > length ys

greedy n xs = gPartition fun n xs where
   cost = sum . map weight
   fit = cost xs `div` n 
   fun ys = fit > cost ys

main :: IO ()
main = do
  cnf <- cmdArgsRun splitter
  eval cnf  

eval :: Splitter -> IO ()
eval cnf = do
  inh <- openFile (file_ cnf) ReadMode
  rows <- hGetContents inh
  let items = map mkItem $ filter ((== 2).length) $ map words $ lines rows
  let numRanges = numranges_ cnf
 
  when (optimal_ cnf) $ do
    let threshold = threshold_ cnf
    let rs = ranges $ optimal numRanges items threshold
    display "Approximation Best" rs
    
  when (greedy_ cnf) $ do   
    let rs = ranges $ greedy numRanges items
    display "Greedy" rs
   
  when (trivial_ cnf) $ do
    let rs = ranges $ trivial numRanges items
    display "Trivial" rs
   
  hClose inh 

-- | Display the results of the Splitter execution 
display :: String -> [Range] -> IO ()
display title ranges = do
   putStrLn $ "\n     " ++ title
   t1 <- getCPUTime
   mapM_ print ranges
   t2 <- getCPUTime
   print $ "   Partition cost = " ++ show (foldr1 max ranges)
   print $ "   Time to execute = " ++ show (div (t2-t1) 1000000000)

-- / Parse items
mkItem :: [String] -> Account
mkItem xs = Item (read $ head xs) (read $ (head . tail) xs)





