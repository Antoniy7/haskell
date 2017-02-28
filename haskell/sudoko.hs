module Sudoku where
import Data.Char
import Data.Array
import Test.QuickCheck
import Data.List
import Data.Matrix
import Data.Maybe

data Sudoku = Sudoku [[Int]] 

example :: Sudoku
example = Sudoku
      [ [3, 6, 0, 0, 7, 1, 2, 0, 0]
      , [0, 5, 0, 0, 0, 0, 1, 8, 0]
      , [0, 0, 9, 2, 0, 4, 7, 0, 0]
      , [0, 0, 0, 0, 1, 3, 0, 2, 8]
      , [4, 0, 0, 5, 0, 2, 0, 0, 9]
      , [2, 7, 0, 4, 6, 0, 0, 0, 0]
      , [0, 0, 5, 3, 0, 8, 9, 0, 0]
      , [0, 8, 3, 0, 0, 0, 0, 6, 0]
      , [0, 0, 7, 6, 9, 0, 0, 4, 3]
      ] 

type Location = (Int, Int)

type Board = Array Location Int

puzzleBoard :: Board
puzzleBoard = array ((0, 0), (8, 8)) $ puzzleAssocs $ rows example  

rows :: Sudoku -> [[Int]] 
rows (Sudoku rs) = rs 

isNotZero :: Int -> Bool 
isNotZero 0       = False 
isNotZero _       = True 

emptyLocations :: Board -> [Location]
emptyLocations board = [(row, col) | row <- [0..8], col <- [0..8], (board ! (row, col)) == 0]

flattenSudoku::Sudoku -> [Int]
flattenSudoku (Sudoku xs) = foldr (++) [] xs

isOK :: Int -> Bool 
isOK 0 = True 
isOK  n 
    | n < 1     = False 
    | n > 9     = False 
    | otherwise = True 

solutions :: Board -> [Board]
solutions board = helper (emptyLocations board) board
  where
    helper :: [Location] -> Board -> [Board]
    helper []     board = [board]
    helper (location:locationss) board = concatMap (helper locationss) possibleBoards
      where
        possibleValues  = [val | val <- [1..9], isPossibleValue val location board]
        possibleBoards = map (\val -> fillBoardWithValues val location board) possibleValues

isPossibleValue :: Int -> Location -> Board -> Bool
isPossibleValue value (row, col) board = notInRow && notInColumn && notInBox
  where
    notInRow    = notElem value $  getValuesInRow board row
    notInColumn = notElem value $  getValuesInColumn board col
    notInBox    = notElem value $  valuesIn3x3Square board (row, col)

fillBoardWithValues :: Int -> Location -> Board -> Board
fillBoardWithValues val (row, col) b = b // [((row, col), val)]

getValuesInRow :: Board -> Int -> [Int]
getValuesInRow b row = [b ! location | location <- range((row, 0), (row, 8))]

getValuesInColumn ::  Board -> Int -> [Int]
getValuesInColumn b col = [b ! location | location <- range((0, col), (8, col))]

valuesIn3x3Square :: Board -> Location -> [Int]
valuesIn3x3Square b (row, col) = [b ! loc | loc <- locations]
  where
    row' = (div row  3) * 3
    col' = (div col  3) * 3
    locations = range((row', col'), (row' + 2, col' + 2))

puzzleAssocs :: [[Int]] -> [(Location, Int)]
puzzleAssocs = concatMap rowAssocs . zip [0..8]
  where
    rowAssocs :: (Int, [Int]) -> [((Int, Int), Int)]
    rowAssocs (row, values) = colAssocs row $ zip [0..8] values

    colAssocs :: Int -> [(Int, Int)] -> [((Int, Int), Int)]
    colAssocs row cols = map (\(col, m) -> ((row, col), m)) cols

headOrNothing :: [a] -> Maybe a
headOrNothing []     = Nothing
headOrNothing (x:xs) = Just x

printBoard :: Maybe Board -> IO ()
printBoard Nothing  = putStrLn "No answer"
printBoard (Just board) = mapM_ putStrLn [show $ getValuesInRow board row | row <- [0..8]]

solve :: Board -> Maybe Board
solve = headOrNothing . solutions

fun :: IO ()
fun = printBoard solution
    where solution = solve puzzleBoard

cell::Gen(Int)
cell = frequency
         [(9, return 0),
         (1, do n <- choose (1,9)
                return n)]

instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_Sudoku::Sudoku -> Bool
prop_Sudoku (Sudoku xs) = isSudoku (Sudoku xs) == True

isSudoku::Sudoku -> Bool
isSudoku (Sudoku xs) 
    |length xs == 9 && (all (== 9) (map length xs)) && numbersCheck (Sudoku xs) = True
    |otherwise = False
      where
            numbersCheck (Sudoku xs) = all (>= 0) (flattenSudoku (Sudoku xs)) && all (<= 9) (flattenSudoku (Sudoku xs))

isSolved :: Sudoku -> Bool 
isSolved s = and [b | r <- [row | row <- (rows s)], p <- [pos | pos <- r],  b <- [isNotZero p] ]
                  
solutionTrivial :: [[Int]]
solutionTrivial = [[ (+ 1).(`mod` 9) . (+ 1) $ ((i * 3) + (i `div` 3) + j) | j <- [1..9] ] | i <- [1..9] ]
