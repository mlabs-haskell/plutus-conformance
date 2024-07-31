module Main (main) where

import Aux (RunTermError, runTerm)
import Complement (cases)
import Control.Exception (throwIO)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Foldable.WithIndex (itraverse_)
import PlutusCore
  ( DefaultFun,
    DefaultUni,
    Name,
    Term,
    TyName,
  )
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  createDirectoryIfMissing False "./output"
  result <- runExceptT . itraverse_ (go "complement") $ cases
  case result of
    Left err -> throwIO . userError . show $ err
    Right () -> pure ()
  where
    go ::
      String ->
      Int ->
      Term TyName Name DefaultUni DefaultFun () ->
      ExceptT RunTermError IO ()
    go label ix = runTerm (label <> "-case-" <> show (ix + 1)) "./output"
