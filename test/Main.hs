module Main (main) where

import MyLib (someFunc)

main :: IO ()
main = do
  someFunc
  error "Test suite not yet implemented."
