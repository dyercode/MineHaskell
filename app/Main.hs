{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bitraversable
import Data.Either.Validation
import Data.List
import Data.Maybe
import MolecularAssembler
  ( Blocks (..),
    LegalAssembler,
    calcBlocksIn,
    calcBlocksNeeded,
    createCube,
    createLegalAssembler,
  )

main :: IO ()
main =
  mapM_
    (print . (\a -> (a, calcBlocksIn a)))
    ( mapMaybe
        unexplain
        [ createCube 3,
          createLegalAssembler 3 3 4,
          createLegalAssembler 3 4 4,
          createCube 4,
          createLegalAssembler 3 3 5,
          createLegalAssembler 3 3 6,
          createLegalAssembler 3 3 7
        ]
    )

findUpDowns :: [((LegalAssembler, LegalAssembler), Blocks)]
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
ohDeargodOld :: [(LegalAssembler, LegalAssembler)]
ohDeargodOld =
  catMaybes
    ( do
        a <- nums
        b <- nums
        return (dm (cd a, cd b))
    )

-- | create all legal assemblers up to size 10,
ohDeargod :: [(LegalAssembler, LegalAssembler)]
ohDeargod =
  mapMaybe
    (bitraverse cd cd)
    ( do
        a <- nums
        b <- nums
        return (a, b)
    )

unexplain :: Validation err a -> Maybe a
unexplain v = case v of
  Success a -> Just a
  Failure _ -> Nothing

cd :: (Int, Int, Int) -> Maybe LegalAssembler
cd (l, w, h) = unexplain (createLegalAssembler l w h)

dm :: (Maybe a, Maybe b) -> Maybe (a, b)
dm (Just a, Just b) = Just (a, b)
dm _ = Nothing
