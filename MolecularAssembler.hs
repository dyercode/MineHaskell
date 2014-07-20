module MolecularAssembler
( createSquare,
  findBlocksNeeded,
  Dimensions,
  Blocks
) where
data Dimensions = Dimensions { l :: Int, w :: Int,  h :: Int } deriving (Show)
data Blocks = Blocks { wall :: Int, vent :: Int, core :: Int } deriving (Show)

createSquare :: Int -> Dimensions
createSquare s = Dimensions s s s

findBlocksNeeded :: Dimensions -> Dimensions -> Blocks
findBlocksNeeded assembler increase = (findBlocksIn (assembler +# increase)) -# (findBlocksIn assembler)

(+#) :: Dimensions -> Dimensions -> Dimensions
(Dimensions al aw ah) +# (Dimensions il iw ih) = Dimensions (al+il) (aw+iw) (ah+ih)

(-#) :: Blocks -> Blocks -> Blocks
(Blocks wall1 vent1 core1) -# (Blocks wall2 vent2 core2) = Blocks (wall1 - wall2) (vent1 - vent2) (core1 - core2)

findWallSize :: Dimensions -> Int
findWallSize (Dimensions l w h)
    | tooSmall l = 0
    | tooSmall w = 0
    | tooSmall h = 0
    | otherwise = (4 * (l+w+h)) - 16
    where tooSmall = (<1)

findVentSize :: Dimensions -> Int
findVentSize (Dimensions l w h)
    | tooManyTooSmall = 0
    | tooSmall l = 2 * (w-2) * (h-2)
    | tooSmall w = 2 * (l-2) * (h-2)
    | tooSmall h = 2 * (l-2) * (w-2)
    | otherwise = 2 * ((l-2) * (w-2) + (w-2) * (h-2) + (l-2) * (h-2))
    where tooSmall = (<2)
          tooManyTooSmall = (length $ filter tooSmall [l,w,h]) > 1

findCoreSize :: Dimensions -> Int
findCoreSize (Dimensions l w h)
    | tooSmall l = 0
    | tooSmall w = 0
    | tooSmall h = 0
    | otherwise = (l - 2) * (w - 2) * (h - 2)
    where tooSmall = (<3)

findBlocksIn :: Dimensions -> Blocks
findBlocksIn a =
	let wall = findWallSize a 
	    vent = findVentSize a
	    core = findCoreSize a
	in Blocks wall vent core
