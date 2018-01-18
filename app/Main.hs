module Main where

import System.Environment (getArgs)
import Data.FGLang (Expr(..), rs, bases)
import Data.Char (isDigit)
import Data.Foldable (toList)
import qualified Data.HashSet as S (toList)

main :: IO ()
main = do
  args <- getArgs
  if (length args /= 1) || (not . null . filter (not . isDigit) . head) args
     then putStrLn "Please provide a single argument: how many iterations of Data.FGLang.rs to print"
     else do
       let n = read (head args)
       mapM_ print . map (map (map (\x -> if x then 'G' else 'F') . toList . getExpr) . S.toList) . take n . iterate rs $ bases

