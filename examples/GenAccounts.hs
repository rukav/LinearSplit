import System.IO
import Data.List (group,sort)
import Test.QuickCheck

data Account = A Int Int 
  deriving (Ord, Show)

instance Eq Account where
  (==) (A i n) (A i' n') = i == i'

instance Arbitrary Account where
   arbitrary = do
      id <- choose (10000, 100000)  
      w <- frequency [ (10, choose (1, 100))
                     , (2, choose (101, 100000))
                     , (2, choose (10001, 1000000))
                     ]
      return $ A id w

maxAccounts = 1000

nubAcc :: [[Account]] -> [[Account]]
nubAcc xs = map (map (foldr1 asum) . group . sort) xs
  where asum (A i n) (A i' n') = A i (n+n')

main = do
  xs <- sample' $ vectorOf maxAccounts (arbitrary :: Gen Account)
  let filename i = "test" ++ show i ++ ".txt"
  mapM_ (\(as,i) -> writeToFile (filename i) as) (zip (nubAcc xs) [1..])


writeToFile name accs =
  do h <- openFile name WriteMode 
     mapM_ (hPutStrLn h) $ 
       map (\(A i w) -> show i ++ "  " ++ show w) accs
     hClose h 