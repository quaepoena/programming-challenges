import Data.Char
import System.IO

data Matrix = Matrix
  { rows :: [String]
  , columns :: [String]
  }

instance Show Matrix where
  show (Matrix r _) = foldl1 (\acc x -> acc ++ "\n" ++ x) r

split :: String -> [String]
split = map (:[])

buildColumns :: [String] -> [String] -> [String]
buildColumns allx@(x:xs) ally@(y:ys) = map (\(x,y) -> x ++ y) $ zip allx ally
buildColumns [] x = x
buildColumns x [] = x

buildMatrix :: [String] -> Matrix
buildMatrix s = Matrix s $ foldl buildColumns [] $ map split s

calculateGamma :: String -> Char
calculateGamma s
  | zeroes > ones = '0'
  | otherwise = '1' where
      zeroes = length $ filter (== '0') s
      ones = length $ filter (== '1') s

binaryToDecimal :: String -> Int
binaryToDecimal s = foldl (\acc (x,y) -> x * y + acc) 0 $ withExponents where
  withExponents = zipWith (\x y -> ((ord x - 48),y)) (reverse s) (map (2^) [0..])

main = do
  contents <- readFile "input"

  let gamma = map calculateGamma . columns . buildMatrix $ lines contents
      epsilon = map (\x -> if x == '0' then '1' else '0')

  print ((binaryToDecimal gamma) * (binaryToDecimal $ epsilon gamma))
