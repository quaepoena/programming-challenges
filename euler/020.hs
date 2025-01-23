-- Simple factorial function.
fac :: Integer -> Integer
fac 0 = 1
fac x = x * fac (x - 1)

-- Convert an integer to a list of its constituent digits.
intToList :: Integer -> [Integer]
intToList = intToList' 1

intToList' :: Integer -> Integer -> [Integer]
intToList' exp x
  | base > x = [curr]
  | exp == 1 = intToList' (exp + 1) (x - rem) ++ [rem]
  | otherwise = intToList' (exp + 1) (x - rem) ++ [curr]
  where base = 10 ^ exp
        rem = x `mod` base
        curr = rem `div` (10 ^ (exp - 1))
        next = x - rem

-- λ> sum . intToList $ fac 100
