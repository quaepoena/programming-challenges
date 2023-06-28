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

criterium :: Char -> Char -> Int -> Int -> Char
criterium true false zeroes ones = if zeroes > ones then true else false

lifeSupport :: [String] -> Int
lifeSupport rows = (binaryToDecimal oxygenRating) * (binaryToDecimal co2Rating) where
  greater = criterium '0' '1'
  lesser = criterium '1' '0'
  oxygenRating = filterRows greater rows 0
  co2Rating = filterRows lesser rows 0

filterRows :: (Int -> Int -> Char) -> [String] -> Int -> String
filterRows _ (x:[]) _ = x
filterRows cri currentRows index = filterRows
                                       cri
                                       (filter (\x -> x !! index == criteriumNumber) currentRows)
                                       (index + 1) where
  currentColumns = foldl buildColumns [] $ map split currentRows
  criteriumNumber = cri zeroes ones
  zeroes = length $ filter (== '0') $ currentColumns !! index
  ones = length $ filter (== '1') $ currentColumns !! index

binaryToDecimal :: String -> Int
binaryToDecimal s = foldl (\acc (x,y) -> x * y + acc) 0 $ withExponents where
  withExponents = zipWith (\x y -> ((ord x - 48),y)) (reverse s) (map (2^) [0..])

main = do
  contents <- readFile "input"

  let m = buildMatrix $ lines contents
      lifeSupportRating = lifeSupport $ rows m

  print lifeSupportRating
