module Othermain (
  otherMain
) where

import Data.Char
import Data.List
import Prelude hiding (map)
import Custom
import Control.Applicative
import Control.Exception

otherMain :: IO ()
otherMain = do
  print (add 2 4)
  print (fact 5)
  putStrLn "hello world"
  print $ m toUpper "william"
  print ((\x -> x + 1) 4)

  putStrLn ((noto.eveno)(16))
  putStrLn ((noto.eveno)(17))
  print (showEven 4)
  print (showBoolean True)
  print . show $ h1 <$> (Just 1) <*> (Just 2)

  let file = "src/Custom.hs"
  contents <- readFile file
  putStrLn contents
  charstuff
  stringstuff
  liststuff
  print $ foobar [1..100]

  print . surface $ Circle 10 20 10

  result <- try $ evaluate (5 `div` 2) :: IO (Either SomeException Int)
  case result of
    Left ex -> putStrLn $ "Cought an exception: " ++ show ex
    Right val -> putStrLn $ "The answer is: " ++ show val

h1 :: Int -> Int -> Int
h1 x y = 2*x+y

eveno :: Int -> Bool
noto :: Bool -> String

eveno x = if x `rem` 2 == 0
  then True
else False

noto x = if x == True
  then "Even num"
else "Odd num"

add :: Integer -> Integer -> Integer
add x y = x + y

fact :: Int -> Int
fact 0 = 1
fact n = n * fact ( n - 1 )

m :: (a -> b) -> [a] -> [b]
m _ [] = []
m func (x: abc) = func x : m func abc

charstuff :: IO ()
charstuff = do
  print (toUpper (toLower 'A'))
  print (toLower (toUpper 'a'))
  print (words "Hello everyone")

stringstuff :: IO ()
stringstuff = do
  putStrLn (intersperse '.' "willdoescode")
  putStrLn (intercalate " " ["I", "am", "eating", "chicken"])
  print    (splitAt 4 "willdoescode")
  print    (sort [2, 1, 4, 3, 6, 5, 8, 7, 10, 9])

liststuff :: IO ()
liststuff = do
  let list = [1..10]
  print (list)
  print (head list)
  print (tail list)
  print (last list)
  print (init list)
  print (null list)
  print (reverse list)
  print (length list)
  print (take 5 ([1..10]))
  print (drop 5 ([1..10]))
  print (maximum [1..10])
  print (minimum [1..10])
  print (sum [1..10])
  print (product [1..10])
  print (elem 5 ([1..5]))
  print (elem 6 ([1..5]))

foobar :: [Int] -> [String]
foobar l = l >>= (\x -> if x `rem` 15 == 0 then ["FooBar"] else if x `rem` 5 == 0 then ["Foo"] else if x `rem` 3 == 0 then ["Bar"] else [show x])

data Area = Circle Float Float Float
surface :: Area -> Float
surface (Circle _ _ r) = pi * r ^ 2
