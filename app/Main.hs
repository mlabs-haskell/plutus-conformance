module Main (main) where

import Aux (runTerm)
import Control.Exception (throwIO)
import Control.Monad.Except (runExceptT)
import CountSetBits (case1)

main :: IO ()
main = do
  result <- runExceptT . runTerm "csb-case-1" "./output" $ case1
  case result of
    Left err -> throwIO . userError . show $ err
    Right () -> pure ()
