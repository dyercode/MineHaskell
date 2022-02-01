module MolecularAssembler
  ( createCube,
    calcBlocksNeeded,
    createLegalAssembler,
    calcBlocksIn,
    Blocks (..),
    Dimensions,
    LegalAssembler,
  )
where

import Data.Bifunctor
import Data.Either.Validation

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
  deriving (Show, Eq)

data LegalAssembler = LegalAssembler Int Int Int
  deriving (Show, Eq)

checkSmallness :: Int -> String -> Validation [String] Int
checkSmallness d name
  | d < 3 = Failure [name ++ " is too small"]
  | otherwise = Success d

-- | Tries to 'Dimensions' enforcing a minimum size of 3x3x3
createLegalAssembler :: Int -> Int -> Int -> Validation [String] LegalAssembler
createLegalAssembler l w h =
  LegalAssembler <$> checkSmallness l "length" <*> checkSmallness w "width" <*> checkSmallness h "height"

-- | alias to createDimensions supplying equal sides
createCube :: Int -> Validation [String] LegalAssembler
createCube s = first (const ["Sides were < 3"]) (createLegalAssembler s s s)

-- | Calculates number of each block type needed to increase 'assembler' to 'desired' size
-- Including refunded blocks when downgrading or changing shape
calcBlocksNeeded :: LegalAssembler -> LegalAssembler -> Blocks
calcBlocksNeeded assembler desired =
  calcBlocksIn desired -# calcBlocksIn assembler

(+#) :: Dimensions -> Dimensions -> Dimensions
(Dimensions al aw ah) +# (Dimensions il iw ih) =
  Dimensions (al + il) (aw + iw) (ah + ih)

(-#) :: Blocks -> Blocks -> Blocks
(Blocks wall1 vent1 core1) -# (Blocks wall2 vent2 core2) =
  Blocks (wall1 - wall2) (vent1 - vent2) (core1 - core2)

calcWallSize :: LegalAssembler -> Int
calcWallSize (LegalAssembler l w h) = (4 * (l + w + h)) - 16

-- | Calculates the area of rectangle less a 1-sized perimiter
lessPlanePerimiter :: Int -> Int -> Int
lessPlanePerimiter a b = (a - 2) * (b - 2)

calcVentSize :: LegalAssembler -> Int
calcVentSize (LegalAssembler l w h) =
  2 * (lessPlanePerimiter l w + lessPlanePerimiter w h + lessPlanePerimiter l h)

calcCoreSize :: LegalAssembler -> Int
calcCoreSize (LegalAssembler l w h) = (l - 2) * (w - 2) * (h - 2)

calcBlocksIn :: LegalAssembler -> Blocks
calcBlocksIn la =
  Blocks {wall = calcWallSize la, vent = calcVentSize la, core = calcCoreSize la}
