-- Simple factorial function.
fac :: Integer -> Integer
fac x = foldl1 (*) [1..x]

-- Convert an integer to a list of its constituent digits.
intToList :: Integer -> [Integer]
intToList = intToList' 1

intToList' :: Integer -> Integer -> [Integer]
intToList' x y
  | base > y = [curr]
  | x == 1 = intToList' (x + 1) next ++ [remainder]
  | otherwise = intToList' (x + 1) next ++ [curr]
  where base = 10 ^ x
        remainder = y `mod` base
        curr = remainder `div` (10 ^ (x - 1))
        next = y - remainder

main :: IO ()
main = do
  print $ sum . intToList $ fac 100
