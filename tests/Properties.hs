-- | Tests for the Utils.LinearSplit module.

module Main where

import Utils.LinearSplit
import Test.QuickCheck
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

-- |
splitters (Split n xs t) = 
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
   let totalCosts = map (floor . sum . map price) (splitters s)
       itemsCost = floor $ sum $ map weight (items s)
   in all (== itemsCost) totalCosts

-- | The optimal algorithm has to produce the lowest partition cost
prop_bestCost s = 
   let (n,xs) = (chunks s, items s)
       maxCost ys = foldr max 0.0 (map price ys)
       partitionCost = floor . maxCost 
       bestCost = partitionCost (lPartition (chunks s) (items s))
   in all (>= bestCost) (map partitionCost (splitters s))

-- | Ensure that the real number of ranges no more than required
prop_numRanges = forAll (arbitrary :: Gen Split) $ \s ->
   all (<= (chunks s)) (map length (splitters s))

-- | Ensure that the splitting dividers are ordered as working items
prop_ordered s = 
  let divs = map (foldr dividers []) (splitters s)
      dividers r xs = if low r == high r then low r : xs
                       else low r : (high r : xs)
   in all (ordered (map item (items s))) divs

-- | Reverse working items preserves the optimal cost
prop_reverse s =
  let (n,xs) = (chunks s, items s)
      maxCost ys = foldr max 0.0 (map price ys)
      partitionCost = floor . maxCost
  in partitionCost (lPartition n xs) == partitionCost (lPartition n (reverse xs))

-- | Ensure that the ranges prices equal to the sum of weight corresponding
-- work items
prop_rangeCost s =
  and [eqCost rs (items s) | rs <- splitters s] 

-- | Testing helpers
ordered :: [Int] -> [Int] -> Bool
ordered [] [] = True
ordered (x:xs) (y:ys) 
   | x == y = ordered xs ys
   | otherwise = ordered xs (y:ys)
ordered _ _ = False

eqCost :: [Range Int Double] -> [Item Int Double] -> Bool
eqCost [] [] = True
eqCost (Range p l h:xs) ys =
        let (ks,zs) = span (\(Item i _) -> i /= h) ys
            (ks',zs') = (ks ++ [head zs], tail zs)
        in and [(item . head) ks' == l
               ,(item . last) ks' == h
               ,floor (sum (map weight ks')) == floor p 
               ,eqCost xs zs'
               ] 

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testProperty "numRanges" prop_numRanges
    , testProperty "ordered" prop_ordered
    , testProperty "reverse" prop_reverse
    , testProperty "totalCost" prop_totalCost
    , testProperty "bestCost" prop_bestCost
    , testProperty "rangeCost" prop_rangeCost
    ]
