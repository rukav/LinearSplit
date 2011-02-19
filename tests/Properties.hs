-- | Tests for the Utils.LinearSplit module.

module Main where

import Data.LinearSplit
import Test.QuickCheck hiding (ranges)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- | A datatype to model the generation the arbitrary splitting processers. 
-- There are the restrictions to items of type Item Int Double 
data Split = Split {
   chunks :: Int,
   items :: [Item Int Double],
   threshold :: Double
} deriving (Show)

-- | A datatype to generate a positive item weight. The weight
-- is restricted to the arbitrary values from 0.0 to 1000000.0
data Weight = W {
  unW :: Double 
}

instance Arbitrary Weight where
   arbitrary = do 
      w <- choose (0.0, 1000000.0)
      return $ W w

instance Arbitrary Split where
   arbitrary = do 
      n <- choose (1,25) :: Gen Int
      ws <- arbitrary :: Gen [Weight]
      thr <- choose (0.0, 10000.0) :: Gen Double
      let mkItem (id, W w) = Item id w
      let is = map mkItem $ zip [1..length ws] ws
      return $ Split n is thr

-- | Different partition strategies to test
splitters (Split n xs t) = --map ranges 
  [lPartition n xs, ltPartition n xs t, byLength n xs, byAvgCost n xs]
  where
    byLength n xs = 
       let fit = length xs `div` n 
           fun ys = fit > length ys
       in gPartition fun n xs
    byAvgCost n xs = 
       let fit = (sum . map weight) xs / fromIntegral n 
           fun ys = fit > (sum . map weight) ys
       in gPartition fun n xs

-- | Ensure that the sum of the items weights equals to 
-- the total ranges costs 
prop_totalCost s = 
   let totalCosts = map (floor . sum . map rangeCost) (splitters s)
       itemsCost = floor $ sum $ map weight (items s)
   in all (== itemsCost) totalCosts

-- | The optimal algorithm has to produce the lowest partition cost
prop_bestCost s = 
   let (n,xs) = (chunks s, items s)
       bestCost = partitionCost (lPartition (chunks s) (items s))
   in all (>= bestCost) $ map partitionCost (splitters s)

-- | Ensure that the real number of ranges no more than required
prop_numRanges = forAll (arbitrary :: Gen Split) $ \s ->
   all (<= chunks s) $ map length (splitters s)

-- | Ensure that the items after splitting the same as the items 
-- before splitting
prop_equal s = 
  let inpIds = map item (items s)
      outIds = concatMap (map item)
  in all (inpIds ==) (map outIds (splitters s))

-- | Reverse working items preserves the optimal cost
prop_reverse s =
  let (n,xs) = (chunks s, items s)
  in partitionCost (lPartition n xs) == 
     partitionCost (lPartition n (reverse xs))

-- | Testing helpers
--rangeCost :: [Item Int Double] -> Double
rangeCost = sum . map weight

partitionCost = floor . maxCost where
   maxCost = foldr (max . rangeCost) 0.0

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testProperty "numRanges" prop_numRanges
    , testProperty "equal" prop_equal
    , testProperty "reverse" prop_reverse
    , testProperty "totalCost" prop_totalCost
    , testProperty "bestCost" prop_bestCost
    ]

