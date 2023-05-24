import Data.List.Split(splitOn)
import qualified Data.Map.Strict as Map
import System.IO(readFile)

type Point = (Int, Int)
type Grid = Map.Map Point Int
type Path = [Point]

vertical :: (Point, Point) -> Bool
vertical ((x1, _), (x2, _)) = x1 == x2

horizontal :: (Point, Point) -> Bool
horizontal ((_, y1), (_, y2)) = y1 == y2

straight :: (Point, Point) -> Bool
straight pair = vertical pair || horizontal pair

visitPoint :: Point -> Grid -> Grid
visitPoint p = Map.insertWith (+) p 1

toPoint :: String -> Point
toPoint s =
  let coordinates = splitOn "," s
      x = head coordinates
      y = last coordinates
  in (read x :: Int, read y :: Int)

-- https://stackoverflow.com/a/66897189
pairwisePoint :: [Point] -> [(Point, Point)]
pairwisePoint [] = []
pairwisePoint [_] = []
pairwisePoint (x:y:xs) = (x, y) : pairwisePoint xs

steps :: Int -> Int -> [Int]
steps a b
  | a < b = [a..b]
  | a > b = reverse [b..a]
  | otherwise = [b]

expandPath :: (Point, Point) -> Path
expandPath pair@((x1, y1), (x2, y2))
  | horizontal pair = zip (steps x1 x2) (repeat y1)
  | vertical pair = zip (repeat x1) (steps y1 y2)
  | otherwise = zip (steps x1 x2) (steps y1 y2)

main :: IO ()
main = do
  input <- readFile "input"

  let pairwisePoints = pairwisePoint
                     . map toPoint
                     . concat . map (splitOn " -> ")
                     $ lines input

      visitedPoints = concat . map expandPath $ filter straight pairwisePoints

      visitedMultiple = length
                      . filter (>=2)
                      . Map.elems
                      $ foldl (\ acc x -> visitPoint x acc) Map.empty visitedPoints

  print visitedMultiple
