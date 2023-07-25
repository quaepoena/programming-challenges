import Data.List(maximum,minimum,sort)
import Data.List.Split(splitOn)
import System.IO(readFile)

score :: Int -> [Int] -> Int
score goal numbers = sum $ map (\ x -> abs (goal - x)) numbers

main :: IO ()
main = do
  input <- readFile "input"

  let numbers = map (\ x -> read x :: Int)
              $ splitOn "," input

      maxNumber = maximum numbers
      minNumber = minimum numbers

      bestScore = head . sort
                $ map (\ x -> score x numbers) [minNumber .. maxNumber]

  print bestScore
