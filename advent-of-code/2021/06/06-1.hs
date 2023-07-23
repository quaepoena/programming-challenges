import Data.List.Split(splitOn)
import System.IO(readFile)

-- This function describes the reproductive behavior of the lantern fish,
-- starting with an idealized fish having a counter of 6, i.e. a fish that will
-- give birth after seven days.
fish :: Int -> Int
fish days
  | days >= 9 = fish (days - 7) + fish (days - 9)
  | days >= 7 = fish (days - 7) + 1
  | otherwise = 1

-- Take the counter from the problem's input and normalize it to run with
-- fish()'s idealized behavior, i.e. add the difference between a full counter
-- and the actual counter.
fishWithCounter :: Int -> Int -> Int
fishWithCounter days counter = fish (days + (6 - counter))

main :: IO ()
main = do

  input <- readFile "input"

  let numbers = map (\ x -> read x :: Int)
              $ splitOn "," input

      days = 80
      totalFish = map (\ x -> fishWithCounter days x) numbers

  print $ sum totalFish
