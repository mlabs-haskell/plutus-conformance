module MyLib (someFunc) where

import PlutusCore.Examples.Data.List

-- basic check if stuff works at all
someFunc :: IO ()
someFunc = putStrLn $ " PlutusCore.Examples.Data.List.omapList: " <> show omapList
