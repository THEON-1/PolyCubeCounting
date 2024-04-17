module Main (main) where

import PolyCube
import Text.Read (readMaybe)
import System.Exit ( ExitCode(ExitFailure), exitWith )
import System.Environment (getArgs)
import qualified Data.HashMap.Strict as Map
import Combinatorics

main :: IO ()
main = do
    maxSize <- parseCLIArgs
    let cube = PolyCube (map (const [Coord3D (0, 0, 0)]) ([1..24]::[Int])) [Coord3D (0, 0, 0)] 1
    let polycubes = Map.singleton (getOffsets $ head $ orientations cube) (size cube, head $ orientations cube)
    let result = evaluateChildren maxSize cube polycubes
    let counts = foldr (increment . fst) [] result
    writeFile "count.out" $ show counts 
    appendFile "count.out" "\n"
    return ()

increment :: Int -> [Int] -> [Int]
increment 1 [] = [1]
increment n [] = 0:increment (n-1) []
increment 1 (x:xs) = (x+1):xs
increment n (x:xs) = x:increment (n-1) xs

parseCLIArgs :: IO Int
parseCLIArgs = do
    args <- getArgs
    let maxSize = readMaybe $ head args :: Maybe Int
    maybe (exitWith $ ExitFailure 1) return maxSize

evaluateChildren :: Int -> PolyCube -> Map.HashMap [Offset3D] (Int, [Coord3D]) -> Map.HashMap [Offset3D] (Int, [Coord3D])
evaluateChildren maxSize polycube = loop (getChildren (maxSize - size polycube) polycube) where
    loop :: [PolyCube] -> Map.HashMap [Offset3D] (Int, [Coord3D]) -> Map.HashMap [Offset3D] (Int, [Coord3D])
    loop [] d = d
    loop ((PolyCube o la s):pcs) d = if any (flip Map.member d . getOffsets) o
        then loop pcs d
        else loop pcs $ evaluateChildren maxSize (PolyCube o la s) $ Map.insert (getOffsets $ head o) (s, head o) d

getChildren :: Int -> PolyCube -> [PolyCube]
getChildren 0 _ = []
getChildren i (PolyCube o la s) = foldr condGrowth [] $ foldr (\x acc -> tuples x possibleGrowth ++ acc) [] [1..i] where
    condGrowth :: [Coord3D] -> [PolyCube] -> [PolyCube]
    condGrowth gs ps = if any (\x -> elem x $ head o) gs
        then ps
        else PolyCube (zipWith (\x n -> insertAllSorted x $ rotateCubes gs n) o [1..24]) gs (s + length gs) : ps
    transformations :: [Offset3D]
    transformations = [
        Offset3D (-1, 0, 0),
        Offset3D (0, -1, 0),
        Offset3D (0, 0, -1),
        Offset3D (1, 0, 0),
        Offset3D (0, 1, 0),
        Offset3D (0, 0, 1)]
    applyTransformations :: Coord3D -> [Offset3D] -> [Coord3D]
    applyTransformations _ [] = []
    applyTransformations p (x:xs) = applyOffset p x:applyTransformations p xs
    possibleGrowth :: [Coord3D]
    possibleGrowth = foldr (\x acc -> insertAllSorted acc (applyTransformations x transformations)) [] la

insertAllSorted :: (Ord a) => [a] -> [a] -> [a]
insertAllSorted [] [] = []
insertAllSorted [] [v] = [v]
insertAllSorted xs [] = xs
insertAllSorted xs (y:ys) = insertAllSorted (insertSorted xs y) ys

insertSorted :: (Ord a) => [a] -> a -> [a]
insertSorted [] v = [v]
insertSorted (x:xs) v
    | v < x     = v:x:xs
    | v > x     = x:insertSorted xs v
    | otherwise = x:xs

