amicDivisors :: Int -> [Int]
amicDivisors n = amicDivisors' 1 n n

-- Find divisors < n.
amicDivisors' :: Int -> Int -> Int -> [Int]
amicDivisors' x y n
  | x >= y = []
  | x == 1 = [x] ++ amicDivisors' (x + 1) (n `quot` x) n
  | x ^ 2 == n = [x]
  | n `mod` x == 0 = [x] ++ amicDivisors' (x + 1) (n `quot` x) n ++ [n `quot` x]
  | otherwise = amicDivisors' (x + 1) y n

-- Return True if x is part of an amicable pair.
amicPar :: Int -> Bool
amicPar x =
  let sumAmic z = sum $ amicDivisors z
      sumX = sumAmic x
      sumY = sumAmic sumX
  in x == sumY && x /= sumX

main :: IO ()
main = do
  print . sum $ filter amicPar [1..10000]
