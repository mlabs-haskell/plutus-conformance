{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module CountSetBits (cases) where

import Data.ByteString (ByteString)
import PlutusCore
  ( DefaultFun,
    DefaultUni,
    Name,
    Term,
    TyName,
  )
import PlutusCore qualified as PLC
import PlutusCore.MkPlc (builtin, mkConstant, mkIterAppNoAnn)

cases :: [Term TyName Name DefaultUni DefaultFun ()]
cases =
  fmap
    (mkIterAppNoAnn (builtin () PLC.CountSetBits))
    [ -- countSetBits [] => 0
      [mkConstant @ByteString () []],
      -- countSetBits [0x00, 0x00] => 0
      [mkConstant @ByteString () [0x00, 0x00]],
      -- countSetBits [0x01, 0x00] => 1
      [mkConstant @ByteString () [0x01, 0x00]],
      -- countSetBits [0x00, 0x01] => 1
      [mkConstant @ByteString () [0x00, 0x01]]
    ]
