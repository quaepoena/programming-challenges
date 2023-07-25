import Data.List(maximum,minimum,sort)
import Data.List.Split(splitOn)
import System.IO(readFile)

ergebnis :: Int -> [Int] -> Int
ergebnis ziel zahlen = sum $ map (\ x -> krabbemotorkraftstoffaufwand (abs (ziel - x))) zahlen

krabbemotorkraftstoffaufwand :: Int -> Int
krabbemotorkraftstoffaufwand = (map kraftstoffaufwand [0..] !!)
  where kraftstoffaufwand 0 = 0
        kraftstoffaufwand n = n + kraftstoffaufwand (n - 1)

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
