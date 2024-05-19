module Main (main) where

import MyLib (someFunc)

main :: IO ()
main = do
  someFunc
  putStrLn "Test suite not yet implemented."
