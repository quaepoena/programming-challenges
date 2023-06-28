import Data.List.Split(splitOn)
import System.IO(readFile)

advanceDay :: [Int] -> Int -> [Int]
advanceDay acc days =
  if days == 8 then acc ++ [6, 8]
  else acc ++ [pred days]

-- https://stackoverflow.com/questions/73161840/run-a-function-a-certain-number-times-in-haskell

main :: IO ()
main = do

  input <- readFile "input-test"

  let numbers = map (\ x -> read x :: Int)
              $ splitOn "," input

  -- take 18 $ iterate (advanceDay []) numbers

  print numbers
