-- |
-- Module      :  Main
-- Copyright   :  (c) Vitaliy Rukavishnikov, 2011
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The LinearSplit module implements partitioning the sequence of items to the 
-- subsequences in the order given. The next functions are exported:
--
-- * gPartition. Split the sequence of items items using greedy heuristic. 
--
-- * lPartition. Split the sequence of items to minimize the maximum cost over 
--   all the subsequences using linear partition algorithm
--   (see the 'The Algorithm Design Manual' by Steven S. Skiena..)  
--
-- * ltPartition. The approximation of the linear partition algorithm.
--   The large size of the work items space is decreased by
--   combining the consecutive items based on the threshold parameter.
-- 

module Data.LinearSplit (
     Item (..)
    ,lPartition
    ,ltPartition
    ,gPartition
) where
import Data.Array 
import Data.List (nub, groupBy, inits)

-- | Representation of the work item
data Item a b = Item {
   item :: a,       -- ^ item id
   weight :: b      -- ^ weight of the item
} deriving (Eq, Show, Ord)

-- | The table cell to store the computed partitions
data Cell b = Cell {
   cost :: b,       -- ^ cost of the partition
   ind  :: Int      -- ^ partition index in the work items
} deriving (Eq, Show, Ord)

-- | Combine the consecutive items to decrease the space of the input
merge :: (Ord b) => b -> Item a b -> Item a b -> Bool
merge i x y = weight x <= i && weight y <= i

-- | Partition the items based on the greedy algoritm
gPartition :: ([Item a b] -> Bool) -> Int -> [Item a b] -> [[Item a b]] 
gPartition f n xs 
  | n <= 0 = gPartition f 1 xs
  | otherwise = go n xs f where
    go _ [] _ = []
    go 1 ys _ = [ys] 
    go n ys f = 
      let cands = dropWhile f ((tail . inits) ys)
          chunk = if null cands then ys else head cands 
          rest = drop (length chunk) ys 
      in chunk : go (n-1) rest f

-- | Partition items with accumulating items 
ltPartition :: (Num b, Ord b) => Int -> [Item a b] -> b -> [[Item a b]]
ltPartition n xs threshold = 
     unshrink $ lPartition n (shrink (merge threshold) xs)

-- | Partition items to minimize the maximum cost over all ranges
lPartition :: (Num b, Ord b) => Int -> [Item a b] -> [[Item a b]]
lPartition size items 
  | size <= 0 = lPartition 1 items
  | otherwise = slices dividers items where
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

-- | Grouping the items
shrink :: Num b => (Item a b -> Item a b -> Bool) -> [Item a b] -> [Item [Item a b] b]
shrink thr items = map mkItem' $ groupBy thr items where
  mkItem' xs = Item xs (sum $ map weight xs)

-- | Ungrouping the items
unshrink :: [[Item [Item a b] b]] -> [[Item a b]]
unshrink = map (concatMap item)


