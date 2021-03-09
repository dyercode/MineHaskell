module Main where

import           MolecularAssembler             ( createCube
                                                , calcBlocksNeeded
                                                )
main :: IO ()
main = case (createCube 3, createCube 4) of
  (Just a , Just b ) -> print (calcBlocksNeeded a b)
  (Nothing, _      ) -> print "first dimensions too small"
  (_      , Nothing) -> print "second dimensions too small"
