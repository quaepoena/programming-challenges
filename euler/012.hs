divisors :: Int -> [Int]
divisors x = divisors' 1 x x

divisors' :: Int -> Int -> Int -> [Int]
divisors' x y z
  | x >= y = []
  | x ^ 2 == z = [x]
  | z `mod` x == 0 = [x] ++ divisors' (x + 1) (z `quot` x) z ++ [z `quot` x]
  | otherwise = divisors' (x + 1) y z

main :: IO ()
main = do
  let triangleNumbers = scanl1 (+) [1..]
  print $ last . head . dropWhile (\x -> length x <= 500) $ map divisors triangleNumbers
