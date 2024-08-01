{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import And qualified
import Aux (RunTermError, runTerm)
import Complement qualified
import Control.Exception (throwIO)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import CountSetBits qualified
import Data.Foldable (traverse_)
import Data.Foldable.WithIndex (ifor_)
import FindFirstSetBit qualified
import Or qualified
import PlutusCore
  ( DefaultFun,
    DefaultUni,
    Name,
    Term,
    TyName,
  )
import ReadBit qualified
import Replicate qualified
import Rotate qualified
import Shift qualified
import System.Directory (createDirectoryIfMissing)
import WriteBits qualified
import Xor qualified

main :: IO ()
main = do
  createDirectoryIfMissing False "./output"
  result <- runExceptT . traverse_ (uncurry go) $ todo
  case result of
    Left err -> throwIO . userError . show $ err
    Right () -> pure ()
  where
    go ::
      String ->
      [Term TyName Name DefaultUni DefaultFun ()] ->
      ExceptT RunTermError IO ()
    go opName ts = do
      let caseDir = "./output/" <> opName
      liftIO . createDirectoryIfMissing False $ caseDir
      ifor_ ts $ \ix -> runTerm (opName <> "-case-" <> show (ix + 1)) caseDir

todo :: [(String, [Term TyName Name DefaultUni DefaultFun ()])]
todo =
  [ ("andbytestring", And.cases),
    ("orbytestring", Or.cases),
    ("xorbytestring", Xor.cases),
    ("complementbytestring", Complement.cases),
    ("readbit", ReadBit.cases),
    ("writebits", WriteBits.cases),
    ("replicatebyte", Replicate.cases),
    ("shiftbytestring", Shift.cases),
    ("rotatebytestring", Rotate.cases),
    ("countsetbits", CountSetBits.cases),
    ("findfirstsetbit", FindFirstSetBit.cases)
  ]
