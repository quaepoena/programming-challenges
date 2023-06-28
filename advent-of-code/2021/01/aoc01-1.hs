import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print (length . increases . map (\x -> read x :: Integer) $ lines contents)
  hClose handle

increases :: [Integer] -> [Bool]
increases (x:y:ys)
  | x < y = True : increases (y:ys)
  | otherwise = increases (y:ys)
increases _ = []
