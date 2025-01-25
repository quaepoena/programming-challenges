-- Naive fib. Could be improved with memoization.
-- https://wiki.haskell.org/Memoization
fib :: Int -> Int
fib n
  | n == 1 || n == 2 = n
  | otherwise = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
  let fibNums = takeWhile (\x -> x < 4000000) $ map fib [1..]
  print $ sum . filter (\x -> x `mod` 2 == 0) $ fibNums
