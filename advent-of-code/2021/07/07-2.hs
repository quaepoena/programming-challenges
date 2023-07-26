import Data.List(maximum,minimum,sort)
import Data.List.Split(splitOn)
import System.IO(readFile)

ergebnis :: Int -> [Int] -> Int
ergebnis ziel zahlen = sum $ map (\ x -> krabbemotorkraftstoffaufwand ((abs (ziel - x)) + 1)) zahlen

krabbemotorkraftstoffaufwand :: Int -> Int
krabbemotorkraftstoffaufwand n = n * (n - 1) `div` 2

main :: IO ()
main = do
  input <- readFile "input"

  let zahlen = map (\ x -> read x :: Int)
              $ splitOn "," input

      maxZahl = maximum zahlen
      minZahl = minimum zahlen

      bestesErgebnis = head . sort
                     $ map (\ x -> ergebnis x zahlen) [minZahl .. maxZahl]

  print bestesErgebnis
