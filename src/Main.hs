module Main where
import Data.Char
import Data.List
import Prelude hiding (map)
import Custom
import Othermain
import Control.Applicative

main :: IO ()
main = do
  print $ multi 5

multi :: Int -> Int
multi x = x * 1
