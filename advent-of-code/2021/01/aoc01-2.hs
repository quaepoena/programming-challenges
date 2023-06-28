import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print (length . increases . map (\x -> read x :: Integer) $ lines contents)
  hClose handle

increases :: [Integer] -> [Bool]
increases (x:y:z:x':x's)
  | x + y + z < y + z + x' = True : increases (y:z:x':x's)
  | otherwise = increases (y:z:x':x's)
increases _ = []
