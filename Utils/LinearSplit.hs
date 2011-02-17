{-# LANGUAGE DeriveDataTypeable   #-}

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
--    a) gPartition  - split the sequence of items items using greedy heuristic. 
--    b) lPartition  - split the sequence of items to minimize the maximum cost over 
--                     all the subsequences using linear partition algorithm
--                     (see the 'The Algorithm Design Manual' by Steven S. Skiena..)  
--    c) ltPartition - the approximation of the linear partition algorithm.
--                     The large size of the work items space is decreased by
--                     combining the consecutive items based on the threshold parameter.
-- 

module Utils.LinearSplit (
    Item (..),
    Range (..),
    lPartition,
    ltPartition,
    gPartition
) where
import Data.Array 
import Data.List (nub, groupBy, inits)

-- | Representation of the work item
data Item a b = Item {
   item :: a,       -- item id
   weight :: b      -- weight of the item
} deriving (Eq, Show, Ord)

-- | Range of work items
data Range a b = Range {
   price :: b,      -- cost of the range
   low :: a,        -- first item of the range
   high :: a        -- last item of the range
} deriving (Eq, Show, Ord)

-- | The table cell to store the computed partitions
data Cell b = Cell {
   cost :: b,       -- cost of the partition
   ind  :: Int      -- partition index in the work items
} deriving (Eq, Show, Ord)

-- | Combine the consecutive items to decrease the space of the input
merge :: (Ord b) => b -> Item a b -> Item a b -> Bool
merge i x y = weight x <= i && weight y <= i

-- | Create ranges
ranges :: (Ord b, Num b) => [[Item a b]] -> [Range a b]
ranges xss =  map mkRange xss where
   mkRange xs = Range (sum $ map weight xs) (item $ head xs) (item $ last xs)

-- | Partition the items based on the greedy algoritm
gPartition :: (Ord b, Num b) => ([Item a b] -> Bool) -> Int -> [Item a b] -> [Range a b]
gPartition fun n = ranges . gPartition' fun n 

gPartition' :: ([Item a b] -> Bool) -> Int -> [Item a b] -> [[Item a b]] 
gPartition' f n xs 
  | n <= 0 = gPartition' f 1 xs
  | otherwise = go n xs f where
    go _ [] _ = []
    go 1 ys _ = [ys] 
    go n ys f = 
      let cands = dropWhile f ((tail . inits) ys)
          chunk = if null cands then ys else head cands 
          rest = drop (length chunk) ys 
      in chunk : go (n-1) rest f

-- | Partition items to minimize the maximum cost over all ranges
lPartition :: (Num b, Ord b) => Int -> [Item a b] -> [Range a b]
lPartition n = ranges . lPartition' n

-- | Partition items with accumulating small items 
ltPartition :: (Num b, Ord b) => Int -> [Item a b] -> b -> [Range a b]
ltPartition n xs threshold = 
     unshrink $ lPartition n (shrink (merge threshold) xs)

lPartition' :: (Num b, Ord b) => Int -> [Item a b] -> [[Item a b]]
lPartition' size items 
  | size <= 0 = lPartition' 1 items
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

-- | Grouping the small items
shrink :: Num b => (Item a b -> Item a b -> Bool) -> [Item a b] -> [Item (a,a) b]
shrink thr items = map mkItem' $ groupBy thr items where
  mkItem' xs = Item (lo xs, hi xs) $ sum $ map weight xs
  lo = item . head
  hi = item . last

-- | Ungrouping the items
unshrink :: [Range (a,a) b] -> [Range a b]
unshrink = map (\(Range cost lo hi) -> Range cost (fst lo) (snd hi))

