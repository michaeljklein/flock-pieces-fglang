module Main where

import Data.Hashable.Orphans (quickCheckHashableSeq)
import Data.FGLang (quickCheckFGLang)
import Test.QuickCheck (quickCheck)
import Data.HashSet.Utils (quickCheckHashSetUtils)

main :: IO ()
main = do
  putStrLn "Data.Hashable.Orphans:"
  print =<< quickCheckHashableSeq

  putStrLn "Data.HashSet.Utils:"
  print =<< quickCheckHashSetUtils

  putStrLn "Data.FGLang:"
  print =<< quickCheckFGLang

