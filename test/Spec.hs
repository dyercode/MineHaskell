module Main
  ( main,
    tests,
  )
where

import Data.Bifunctor (second)
import Data.Either.Validation
import qualified Data.Either.Validation as Data.Validation
import Hedgehog (Gen, MonadTest, Property, assert, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import MolecularAssembler
  ( Blocks (core, vent, wall),
    calcBlocksIn,
    createCube,
    createLegalAssembler,
  )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, hedgehogTests]

genValidDims :: Gen Int
genValidDims =
  Gen.int (Range.linear 3 1000)

genPositiveInt :: Gen Int
genPositiveInt =
  Gen.int (Range.linear 0 10000)

allEqual :: Eq a => [a] -> Bool
allEqual xs = all (== head xs) (tail xs)

test_property_dimensions_interchangeable :: Property
test_property_dimensions_interchangeable =
  property $ do
    l <- forAll genValidDims
    w <- forAll genValidDims
    h <- forAll genValidDims
    test_block_count_dimmension_commutative l w h

test_dimmension_growth :: Property
test_dimmension_growth =
  property $ do
    l <- forAll genValidDims
    w <- forAll genValidDims
    h <- forAll genValidDims
    lGrowth <- forAll genPositiveInt
    wGrowth <- forAll genPositiveInt
    hGrowth <- forAll genPositiveInt
    test_property_scaling l w h lGrowth wGrowth hGrowth

hedgehogTests :: TestTree
hedgehogTests =
  testGroup
    "Property Tests"
    [ testProperty
        "dimmensions are interchangeable in regards to block count"
        test_property_dimensions_interchangeable,
      testProperty
        "cores grow faster than vents grow faster than walls"
        test_dimmension_growth
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testCase "cube creates dimensions with same size sides" $
        createCube 3 @?= createLegalAssembler 3 3 3,
      testCase "cube does not work when size too small" $
        createCube 2 @?= Failure ["Sides were < 3"],
      testCase "Dimensions must say which dims were wrong" $ do
        createLegalAssembler 1 2 3 @?= Failure ["length is too small", "width is too small"]
        createLegalAssembler 3 2 1 @?= Failure ["width is too small", "height is too small"]
    ]

compareAllBlocks :: Int -> Int -> Int -> Bool
compareAllBlocks a b c =
  let creater (a', b', c') = createLegalAssembler a' b' c'
      farter = Data.Validation.validationToEither . second calcBlocksIn . creater
      thing = mapM farter [(a, b, c), (a, c, b), (b, a, c), (b, c, a), (c, a, b), (c, b, a)]
   in case thing of
        Right x -> allEqual x
        Left _ -> False

test_block_count_dimmension_commutative :: (MonadTest m) => Int -> Int -> Int -> m ()
test_block_count_dimmension_commutative a b c =
  Hedgehog.assert (compareAllBlocks a b c)

floatDiv :: Integral a => a -> a -> Double
floatDiv a b = fromIntegral a / fromIntegral b

compareGrowth :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
compareGrowth l w h gl gw gh =
  (gl + gw + gh) < 1
    || ( let block = createLegalAssembler l w h
             growth = createLegalAssembler (l + gl) (w + gw) (h + gh)
          in ( case (block, growth) of
                 (Success bl, Success gr) ->
                   let before = calcBlocksIn bl
                       grown = calcBlocksIn gr
                       wallScale = floatDiv (wall grown) (wall before)
                       ventScale = floatDiv (vent grown) (vent before)
                       coreScale = floatDiv (core grown) (core before)
                    in wallScale < ventScale && ventScale < coreScale
                 _ -> False
             )
       )

test_property_scaling :: MonadTest m => Int -> Int -> Int -> Int -> Int -> Int -> m ()
test_property_scaling l w h gl gw gh =
  Hedgehog.assert (compareGrowth l w h gl gw gh)
