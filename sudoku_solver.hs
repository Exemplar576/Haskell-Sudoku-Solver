import Data.List

type Board = Int
type Index = Int
type Range = [Int]

--Main Solving Activity
main :: IO()
main = do
    let board =[[5,3,0,0,7,0,0,0,0],
                [6,0,0,1,9,5,0,0,0],
                [0,9,8,0,0,0,0,6,0],
                [8,0,0,0,6,0,0,0,3],
                [4,0,0,8,0,3,0,0,1],
                [7,0,0,0,2,0,0,0,6],
                [0,6,0,0,0,0,2,8,0],
                [0,0,0,4,1,9,0,0,5],
                [0,0,0,0,8,0,0,7,9]]
    let constants = getConstantValues board [0..8]
    putStrLn $ printBoard board
    putStrLn $ printBoard $ solveloop board constants (0,0) 1

solveloop :: [[Board]] -> [(Index, Index)] -> (Index, Index) -> Int -> [[Board]]
solveloop board constants (x,y) count = do
    let possible = possibleValues board (x,y)
    if isSolved board
        then board
    else if length (possible) == 1 && length (possible) == count && not (elem (x,y) constants)
        then loopSolve (updateValues board (x,y) (possible!!0) [0..8]) constants (0, 0) count
    else if length (possible) > 1 && length (possible) == count && not (elem (x,y) constants)
        then backTrackAlgo board constants (x,y) possible
    else loopSolve board constants (x,y) count
    where
    loopSolve board constants (x,y) count
        | y < 8 = solveloop board constants (x,y+1) count
        | x < 8 = solveloop board constants (x+1,0) count
        | count == 9 = solveloop board constants (0,0) 1
        | otherwise = solveloop board constants (0,0) (count + 1)

backTrackAlgo :: [[Board]] -> [(Index, Index)] -> (Index, Index) -> [Int] -> [[Board]]
backTrackAlgo board constants (x,y) (possible:xs)
    | isSolved solve = solve
    | otherwise = backTrackAlgo board constants (x,y) xs
    where solve = solveloop (updateValues board (x,y) possible [0..8]) constants (0,0) 1

possibleValues :: [[Board]] -> (Index, Index) -> [Int]
possibleValues board (x, y) = filter (\a -> not $ elem a (getColumnbyIndex board x ++ getRowbyIndex board y ++ getBoxbyCoord board (x,y))) [1,2,3,4,5,6,7,8,9]

updateValues :: [[Board]] -> (Index, Index) -> Int -> Range -> [[Board]]
updateValues _ _ _ [] = []
updateValues board (x, y) value (xz:xs)
    | xz == (8-y) = updateValue (board!!(8-y)) x value [0..8] : updateValues board (x, y) value xs
    | otherwise = board!!xz : updateValues board (x, y) value xs
    where 
        updateValue _ _ _ [] = []
        updateValue row index value (x:xs)
            | index == x = value : updateValue row index value xs
            | otherwise = row!!x : updateValue row index value xs

--Print and Check Functions
isSolved :: [[Board]] -> Bool
isSolved board = not $ elem 0 $ concat board

printBoard :: [[Board]] -> String
printBoard board = concat(intersperse "\n" $ map (\x -> concat(intersperse " " (map show x))) board) ++ "\n"

--Essential board data
getValuebyCoord :: [[Board]] -> (Index, Index) -> Int
getValuebyCoord board (x, y) = (getRowbyIndex board y)!!x

getRowbyIndex :: [[Board]] -> Index -> [Int]
getRowbyIndex board index = take 9 (drop (index * 9) (concat $ reverse board))

getColumnbyIndex :: [[Board]] -> Index -> [Int]
getColumnbyIndex [] _ = []
getColumnbyIndex (x:xs) index = x!!index : getColumnbyIndex xs index

getBoxbyCoord :: [[Board]] -> (Index, Index) -> [Int]
getBoxbyCoord board (x, y) = map (\a -> getValuebyCoord board a) (filter (/= (x,y)) (boxDefinition (x,y)))

boxDefinition :: (Index, Index) -> [(Index, Index)]
boxDefinition (x, y)
    | x <= 2 && y <= 2 = [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]
    | x <= 2 && y <= 5 = [(0,3),(0,4),(0,5),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5)]
    | x <= 2 && y <= 8 = [(0,6),(0,7),(0,8),(1,6),(1,7),(1,8),(2,6),(2,7),(2,8)]
    | x <= 5 && y <= 2 = [(3,0),(4,1),(5,2),(3,0),(4,1),(5,2),(3,0),(4,1),(5,2)]
    | x <= 5 && y <= 5 = [(3,3),(3,4),(3,5),(4,3),(4,4),(4,5),(5,3),(5,4),(5,5)]
    | x <= 5 && y <= 8 = [(3,6),(3,7),(3,8),(4,6),(4,7),(4,8),(5,6),(5,7),(5,8)]
    | x <= 8 && y <= 2 = [(6,0),(6,1),(6,2),(7,0),(7,1),(7,2),(8,0),(8,1),(8,2)]
    | x <= 8 && y <= 5 = [(6,3),(6,4),(6,5),(7,3),(7,4),(7,5),(8,3),(8,4),(8,5)]
    | x <= 8 && y <= 8 = [(6,6),(6,7),(6,8),(7,6),(7,7),(7,8),(8,6),(8,7),(8,8)]

--Constant Values
getConstantValues :: [[Board]] -> Range -> [(Int, Int)]
getConstantValues _ [] = []
getConstantValues board (x:xs)
    | x > 8 = []
    | otherwise = getConstantValues board xs ++ constantIndex board (getRowbyIndex board x) x [0..8]
    where
    constantIndex _ _ _ [] = []
    constantIndex board row ycoord (x:xs)
        | row!!x /= 0 = (x, ycoord) : constantIndex board row ycoord xs
        | otherwise = constantIndex board row ycoord xs