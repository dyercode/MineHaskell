module Main where

import MolecularAssembler (findBlocksNeeded, createSquare)

main :: IO ()
main = print (findBlocksNeeded (createSquare 3) (createSquare 4))
