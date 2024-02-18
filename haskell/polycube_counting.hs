import Data.HashMap.Strict

newtype Coord3D = Coord3D (Int, Int, Int)
    deriving Show

newtype Offset3D = Offset3D (Int, Int, Int)
    deriving Show

data PolyCube = PolyCube {
    orientations :: [[Coord3D]],
    lastAdded :: [Coord3D]
}

main :: IO ()
main = do
    let cube = PolyCube (map (const $ Cubes [Coord3D (0, 0, 0)]) [1..24]) [Coord3D (0, 0, 0)]
    let polycubes = Map.singleton (getOffsets $ head $ orientations cube) head orientations cube
    return ()

getOffsets :: [Coord3D] -> [Offset3D]
getOffsets [x] = []
getOffsets (x:y:xs) = diff x y:getOffsets (y:xs)

diff :: Coord3D -> Coord3D -> Offset3D
diff (Coord3D (a, b, c)) (Coord3D (x, y, z)) = Offset3D (x-a, y-b, z-c)

rotateCubes :: Cubes -> Int -> Cubes
rotateCubes (Cubes tree) i = Cubes $ map (invert . flip . rotate i) tree where
    rotate ::  Int -> Coord3D -> (Int, (Int, Int, Int))
    rotate i (Coord3D (a, b, c)) = case mod i 3 of
        0 -> (j, (a, b, c))
        1 -> (j, (b, c, a))
        2 -> (j, (c, a, b))
        where
            j = div i 3
    flip :: (Int, (Int, Int, Int)) -> (Int, (Int, Int, Int))
    flip (i, (a, b, c)) = case mod i 4 of
        0 -> (j, (a, b, c))
        1 -> (j, (a, -b, -c))
        2 -> (j, (-a, b, -c))
        3 -> (j, (-a, -b, c))
        where
            j = div i 4
    invert :: (Int, (Int, Int, Int)) -> Coord3D
    invert (i, (a, b, c)) = case mod i 2 of
        0 -> Coord3D (a, b, c)
        1 -> Coord3D (-c, -b, -a)
