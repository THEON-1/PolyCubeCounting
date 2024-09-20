{-# LANGUAGE DeriveGeneric #-}
module PolyCube (
    Coord3D (..),
    Offset3D (..),
    PolyCube (..),
    getOffsets,
    applyOffset,
    diff,
    rotateCubes
) where

import Prelude hiding (flip)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

newtype Coord3D = Coord3D (Int, Int, Int)
    deriving (
        Show,
        Eq,
        Generic,
        Ord
    )
instance Hashable Coord3D

newtype Offset3D = Offset3D (Int, Int, Int)
    deriving (
        Show,
        Eq,
        Generic,
        Ord
    )
instance Hashable Offset3D

data PolyCube = PolyCube {
    orientations :: [[Coord3D]],
    lastAdded :: [Coord3D],
    size :: Int
} deriving (
    Show,
    Eq
    )

-- offset calculations
getOffsets :: [Coord3D] -> [Offset3D]
getOffsets [] = error "empty List"
getOffsets [_] = []
getOffsets (x:y:xs) = diff x y:getOffsets (y:xs)

diff :: Coord3D -> Coord3D -> Offset3D
diff (Coord3D (a, b, c)) (Coord3D (x, y, z)) = Offset3D (x-a, y-b, z-c)

applyOffset :: Coord3D -> Offset3D -> Coord3D
applyOffset (Coord3D (a, b, c)) (Offset3D (x, y, z)) = Coord3D (a+x, b+y, c+z)

-- calculates an oriented reprenentation
rotateCubes :: [Coord3D] -> Int -> [Coord3D]
rotateCubes ors n = map (invert . flip . rotate n) ors where
    rotate ::  Int -> Coord3D -> (Int, (Int, Int, Int))
    rotate i (Coord3D (a, b, c)) = case mod i 3 of
        0 -> (j, (a, b, c))
        1 -> (j, (b, c, a))
        2 -> (j, (c, a, b))
        _ -> (j, (a, b, c))
        where
            j = div i 3
    flip :: (Int, (Int, Int, Int)) -> (Int, (Int, Int, Int))
    flip (i, (a, b, c)) = case mod i 4 of
        0 -> (j, (a, b, c))
        1 -> (j, (a, -b, -c))
        2 -> (j, (-a, b, -c))
        3 -> (j, (-a, -b, c))
        _ -> (j, (a, b, c))
        where
            j = div i 4
    invert :: (Int, (Int, Int, Int)) -> Coord3D
    invert (i, (a, b, c)) = case mod i 2 of
        0 -> Coord3D (a, b, c)
        1 -> Coord3D (-c, -b, -a)
        _ -> Coord3D (a, b, c)

