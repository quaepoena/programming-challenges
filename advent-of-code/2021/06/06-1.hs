import Data.List.Split(splitOn)
import System.IO(readFile)

fish :: Int -> Int
fish days
  | days >= 9 = fish (days - 7) + fish (days - 9)
  | days >= 7 = fish (days - 7) + 1
  | otherwise = 1

fishWithCounter :: Int -> Int -> Int
fishWithCounter days counter = fish (days + (6 - counter))

main :: IO ()
main = do

  input <- readFile "input"

  let numbers = map (\ x -> read x :: Int)
              $ splitOn "," input

      days = 80
      withCounter = fishWithCounter days
      totalFish = map withCounter numbers

  print $ sum totalFish
