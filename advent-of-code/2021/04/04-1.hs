import Data.List(sort, transpose)
import Data.List.Split(splitOn)
import System.IO(readFile)

newtype Matrix a = Matrix { rows :: [[a]] } deriving (Eq, Show)
instance Functor Matrix where
  fmap f m = Matrix . map (\ x -> map f x) $ rows m

type BingoSquare = (Int, Bool)
type BingoBoard = Matrix BingoSquare
type BingoBoards = [BingoBoard]

columns :: Matrix a -> [[a]]
columns m = transpose $ rows m

createBoards :: [[String]] -> BingoBoards
createBoards [] = []
createBoards inputLines =
  let toBingoBoard = fmap (\ x -> (read x :: Int, False))
  in (toBingoBoard . Matrix $ take 5 inputLines) : (createBoards $ drop 5 inputLines)

playNumber :: Int -> BingoSquare -> BingoSquare
playNumber num (a, b) =
  if num == a then (a, True) else (a, b)

score :: Int -> BingoBoard -> Int
score num board =
  let toScores (a, b) = if b == False then a else 0
      squareScores = sum . concat . rows $ fmap toScores board
  in num * squareScores

winner :: BingoBoard -> Bool
winner board =
  let checkRow = all (\ (_, b) -> b == True)
  in any checkRow $ rows board ++ columns board

-- Play a round, sort the new winners out such that we can retrieve the last winner.
bingoRound :: ([Int], BingoBoards) -> Int -> ([Int], BingoBoards)
bingoRound (previousWinners, boards) number =
  let newBoards = map (\ board -> fmap (playNumber number) board) boards
      newWinners = previousWinners ++ (sort . map (score number) $ filter winner newBoards)
  in (newWinners, filter (not . winner) newBoards)

main :: IO ()
main = do
  input <- readFile "input"

  let contents = filter (/= "") $ lines input

      numbers = map (\ x -> read x :: Int) . splitOn "," $ head contents
      boards = createBoards . map (\ x -> words x) $ tail contents

      lastWinner = head . fst
                 . head
                 . dropWhile (\ x -> fst x == [])
                 $ scanl bingoRound ([], boards) numbers

  print lastWinner
