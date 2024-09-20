module Children (
    getChildren
) where

import PolyCube
import Combinatorics

-- generates legal children of given polycube up to a given size
getChildren :: Int -> PolyCube -> [PolyCube]
getChildren 0 _ = []
getChildren i (PolyCube o la s) = foldr condGrowth [] $ foldr (\x acc -> tuples x possibleGrowth ++ acc) [] [1..i] where
    condGrowth :: [Coord3D] -> [PolyCube] -> [PolyCube]
    condGrowth gs ps =
        if any (\x -> elem x $ head o) gs
        then ps
        else PolyCube (zipWith (\x n -> insertAllSorted x $ rotateCubes gs n) o [1..24]) gs (s + length gs) : ps
    possibleGrowth :: [Coord3D]
    possibleGrowth = foldr (\x acc -> insertAllSorted acc (generateNeighbors x)) [] la

-- generate all neighbors of a Coord3D
generateNeighbors :: Coord3D -> [Coord3D]
generateNeighbors p = map (applyOffset p) transformations where
    transformations :: [Offset3D]
    transformations = [
        Offset3D (-1, 0, 0),
        Offset3D (0, -1, 0),
        Offset3D (0, 0, -1),
        Offset3D (1, 0, 0),
        Offset3D (0, 1, 0),
        Offset3D (0, 0, 1)]

-- inserts all elements from the second list into the first
insertAllSorted :: (Ord a) => [a] -> [a] -> [a]
insertAllSorted [] [] = []
insertAllSorted [] [v] = [v]
insertAllSorted xs [] = xs
insertAllSorted xs (y:ys) = insertAllSorted (insertSorted xs y) ys

-- inserts element into sorted list
insertSorted :: (Ord a) => [a] -> a -> [a]
insertSorted [] v = [v]
insertSorted (x:xs) v
    | v < x     = v:x:xs
    | v > x     = x:insertSorted xs v
    | otherwise = x:xs

