module Main where

import qualified Data.Flip (quickTestBatches)
import Data.Hashable.Orphans (quickCheckHashableSeq)
import Data.FGLang (quickCheckFGLang)
import Test.QuickCheck (quickCheck)
import Data.HashSet.Utils (quickCheckHashSetUtils)

main :: IO ()
main = do
  putStrLn "Data.Flip:"
  Data.Flip.quickTestBatches

  putStrLn "Data.Hashable.Orphans:"
  print =<< quickCheckHashableSeq

  putStrLn "Data.HashSet.Utils:"
  print =<< quickCheckHashSetUtils

  putStrLn "Data.FGLang:"
  print =<< quickCheckFGLang

