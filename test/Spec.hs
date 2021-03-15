module Main
  ( main,
    tests,
  )
where

-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC

import Data.Either.Validation
import MolecularAssembler
  ( createCube,
    createLegalAssembler,
  )
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

-- properties :: TestTree
-- properties = testGroup "Properties" [scProps, qcProps]

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
