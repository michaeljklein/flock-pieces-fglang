module Main where

import qualified Data.Flip (quickTestBatches)
import Data.Hashable.Orphans (testHashableSeqIO)
import Scratch.FGLang (quickCheckFGLang)
import Test.QuickCheck (quickCheck)
import Data.HashSet.Utils (quickCheckHashSetUtils)

main :: IO ()
main = do
  putStrLn "Data.Flip:"
  Data.Flip.quickTestBatches

  putStrLn "Data.Hashable.Orphans:"
  testHashableSeqIO

  putStrLn "Scratch.FGLang:"
  print =<< quickCheckFGLang

  putStrLn "Data.HashSet.Utils:"
  print =<< quickCheckHashSetUtils

