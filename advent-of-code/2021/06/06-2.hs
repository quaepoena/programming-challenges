import Data.List.Split(splitOn)
import System.IO(readFile)

memoizedFish :: Int -> Int
memoizedFish = (map fish [0 ..] !!)
  where fish 0 = 1
        fish 1 = 1
        fish 2 = 1
        fish 3 = 1
        fish 4 = 1
        fish 5 = 1
        fish 6 = 1
        fish 7 = 2
        fish 8 = 2
        fish n = memoizedFish (n - 7) + memoizedFish (n - 9)

fishWithCounter :: Int -> Int -> Int
fishWithCounter days counter = memoizedFish (days + (6 - counter))

main :: IO ()
main = do

  input <- readFile "input"

  let numbers = map (\ x -> read x :: Int)
              $ splitOn "," input

      days = 256
      withCounter = fishWithCounter days
      totalFish = map withCounter numbers

  print $ sum totalFish
