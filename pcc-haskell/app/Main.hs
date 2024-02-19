module Main (main) where

import PolyCube
import qualified Data.HashMap.Strict as Map

main :: IO ()
main = do
    let cube = PolyCube (map (const $ [Coord3D (0, 0, 0)]) ([1..24]::[Int])) [Coord3D (0, 0, 0)]
    let polycubes = Map.singleton (getOffsets $ head $ orientations cube) (head $ orientations cube)
    return ()
