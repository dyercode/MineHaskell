module MolecularAssembler
  ( createCube,
    calcBlocksNeeded,
    createLegalAssembler,
    calcBlocksIn,
    Blocks (..),
    Dimensions,
  )
where

data Dimensions = Dimensions
  { length :: Int,
    width :: Int,
    height :: Int
  }
  deriving (Show, Eq)

data Blocks = Blocks
  { wall :: Int,
    vent :: Int,
    core :: Int
  }
  deriving (Show)

-- could be either spitting out which dim is wrong, but just want to get working for now

-- | Tries to 'Dimensions' enforcing a minimum size of 3x3x3
createLegalAssembler :: Int -> Int -> Int -> Maybe Dimensions
createLegalAssembler l w h
  | tooSmall l = Nothing
  | tooSmall w = Nothing
  | tooSmall h = Nothing
  | otherwise = Just (Dimensions l w h)
  where
    tooSmall = (< 3)

-- | alias to createDimensions supplying equal sides
createCube :: Int -> Maybe Dimensions
createCube s = createLegalAssembler s s s

-- | Calculates number of each block type needed to increase 'assembler' to 'desired' size
-- Including refunded blocks when downgrading or changing shape
calcBlocksNeeded :: Dimensions -> Dimensions -> Blocks
calcBlocksNeeded assembler desired =
  calcBlocksIn desired -# calcBlocksIn assembler

(+#) :: Dimensions -> Dimensions -> Dimensions
(Dimensions al aw ah) +# (Dimensions il iw ih) =
  Dimensions (al + il) (aw + iw) (ah + ih)

(-#) :: Blocks -> Blocks -> Blocks
(Blocks wall1 vent1 core1) -# (Blocks wall2 vent2 core2) =
  Blocks (wall1 - wall2) (vent1 - vent2) (core1 - core2)

calcWallSize :: Dimensions -> Int
calcWallSize (Dimensions l w h) = (4 * (l + w + h)) - 16

-- | Calculates the area of rectangle less a 1-sized perimiter
lessPlanePerimiter :: Int -> Int -> Int
lessPlanePerimiter a b = (a - 2) * (b - 2)

calcVentSize :: Dimensions -> Int
calcVentSize (Dimensions l w h) =
  2 * (lessPlanePerimiter l w + lessPlanePerimiter w h + lessPlanePerimiter l h)

calcCoreSize :: Dimensions -> Int
calcCoreSize (Dimensions l w h) = (l - 2) * (w - 2) * (h - 2)

calcBlocksIn :: Dimensions -> Blocks
calcBlocksIn d =
  Blocks {wall = calcWallSize d, vent = calcVentSize d, core = calcCoreSize d}
