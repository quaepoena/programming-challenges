import Data.List.Split
import System.IO

data Command = Command Direction Int deriving (Show)
data Direction = Down | Forward | Up deriving (Show)

data Position = Position HorizontalP VerticalP deriving (Show)
type HorizontalP = Int
type VerticalP = Int

vertical :: Position -> VerticalP
vertical (Position _ x) = x

horizontal :: Position -> HorizontalP
horizontal (Position x _) = x

direction :: Command -> Direction
direction (Command x _) = x

distance :: Command -> Int
distance (Command _ x) = x

followCourse :: Command -> Position -> Position
followCourse com pos = case direction com of Forward -> Position (horizontal pos + distance com) (vertical pos)
                                             Down -> Position (horizontal pos) (vertical pos + distance com)
                                             Up -> Position (horizontal pos) (vertical pos - distance com)

parseCommand :: String -> Command
parseCommand s
  | command == "forward" = Command Forward distance
  | command == "up" = Command Up distance
  | command == "down" = Command Down distance where
      command = head $ splitOn " " s
      distance = read (last $ splitOn " " s) :: Int
parseCommand _ = Command Forward 0

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print . foldl (\acc x -> followCourse x acc) (Position 0 0) . map parseCommand $ lines contents
  hClose handle
