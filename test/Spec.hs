module Main
  ( main,
    tests,
  )
where

-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord
import MolecularAssembler
  ( createCube,
    createDimensions,
  )
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

-- properties :: TestTree
-- properties = testGroup "Properties" [scProps, qcProps]

unitTests =
  testGroup
    "Unit Tests"
    [ testCase "cube creates dimensions with same size sides" $
        createCube 3 @?= createDimensions 3 3 3,
      testCase "cube does not work when size too small" $
        createCube 2 @?= Nothing
    ]
