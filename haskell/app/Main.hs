module Main (main) where

import PolyCube
import Children
import Text.Read (readMaybe)
import System.Exit ( ExitCode(ExitFailure), exitWith )
import System.Environment (getArgs)
import qualified Data.HashMap.Strict as Map

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
    loop ((PolyCube o la s):pcs) d = if any (\x -> Map.member (getOffsets x) d) o
        then loop pcs d
        else loop pcs $ evaluateChildren maxSize (PolyCube o la s) $ Map.insert (getOffsets $ head o) (s, head o) d

