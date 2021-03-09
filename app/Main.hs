module Main where

import Data.Bitraversable
import Data.List
import Data.Maybe
import MolecularAssembler
  ( Blocks (..),
    Dimensions,
    calcBlocksNeeded,
    createCube,
    createLegalAssembler,
  )

main :: IO ()
main =
  -- do
  -- mapM_ print findUpDowns
  case (createCube 3, createCube 4) of
    (Just a, Just b) -> print (calcBlocksNeeded a b)
    (Nothing, _) -> print "first dimensions too small"
    (_, Nothing) -> print "second dimensions too small"

findUpDowns :: [((Dimensions, Dimensions), Blocks)]
findUpDowns =
  let pairsWithNeeded = map (\p -> (p, uncurry calcBlocksNeeded p)) ohDeargod
   in filter (isUpAndDown . snd) pairsWithNeeded

data UpDown = Positive | Negative deriving (Eq)

isPos :: Int -> UpDown
isPos i = if i >= 0 then Positive else Negative

isUpAndDown :: Blocks -> Bool
isUpAndDown Blocks {wall = w, vent = v, core = c} =
  let posNegs = [isPos w, isPos v, isPos c]
   in elem Positive posNegs && elem Negative posNegs

nums :: [(Int, Int, Int)]
nums =
  let range = enumFromTo 3 10
   in nub $
        sort $ do
          a <- range
          b <- range
          c <- range
          return (a, b, c)

-- | create all legal assemblers up to size 10,
ohDeargodOld :: [(Dimensions, Dimensions)]
ohDeargodOld =
  catMaybes
    ( do
        a <- nums
        b <- nums
        return (dm (cd a, cd b))
    )

-- | create all legal assemblers up to size 10,
ohDeargod :: [(Dimensions, Dimensions)]
ohDeargod =
  mapMaybe
    (bitraverse cd cd)
    ( do
        a <- nums
        b <- nums
        return (a, b)
    )

cd :: (Int, Int, Int) -> Maybe Dimensions
cd (l, w, h) = createLegalAssembler l w h

dm :: (Maybe a, Maybe b) -> Maybe (a, b)
dm (Just a, Just b) = Just (a, b)
dm _ = Nothing
