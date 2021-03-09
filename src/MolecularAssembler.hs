module MolecularAssembler
  ( createCube
  , calcBlocksNeeded
  , createDimensions
  , Blocks
  ) where

data Dimensions = Dimensions
  { length :: Int
  , width  :: Int
  , height :: Int
  }
  deriving (Show, Eq)

data Blocks = Blocks
  { wall :: Int
  , vent :: Int
  , core :: Int
  }
  deriving Show

-- could be either spitting out which dim is wrong, but just want to get working for now
createDimensions :: Int -> Int -> Int -> Maybe Dimensions
createDimensions l w h | tooSmall l = Nothing
                       | tooSmall w = Nothing
                       | tooSmall h = Nothing
                       | otherwise  = Just (Dimensions l w h)
  where tooSmall = (< 3)

createCube :: Int -> Maybe Dimensions
createCube s = createDimensions s s s

calcBlocksNeeded :: Dimensions -> Dimensions -> Blocks
calcBlocksNeeded assembler increase =
  calcBlocksIn (assembler +# increase) -# calcBlocksIn assembler

(+#) :: Dimensions -> Dimensions -> Dimensions
(Dimensions al aw ah) +# (Dimensions il iw ih) =
  Dimensions (al + il) (aw + iw) (ah + ih)

(-#) :: Blocks -> Blocks -> Blocks
(Blocks wall1 vent1 core1) -# (Blocks wall2 vent2 core2) =
  Blocks (wall1 - wall2) (vent1 - vent2) (core1 - core2)

calcWallSize :: Dimensions -> Int
calcWallSize (Dimensions l w h) = (4 * (l + w + h)) - 16

calcVentSize :: Dimensions -> Int
calcVentSize (Dimensions l w h) =
  2 * ((l - 2) * (w - 2) + (w - 2) * (h - 2) + (l - 2) * (h - 2))

calcCoreSize :: Dimensions -> Int
calcCoreSize (Dimensions l w h) = (l - 2) * (w - 2) * (h - 2)

calcBlocksIn :: Dimensions -> Blocks
calcBlocksIn d = Blocks (calcWallSize d) (calcVentSize d) (calcCoreSize d)
