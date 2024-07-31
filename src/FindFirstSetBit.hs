{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module FindFirstSetBit (cases) where

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
    (mkIterAppNoAnn (builtin () PLC.FindFirstSetBit))
    [ -- findFirstSetBit [] => -1
      [mkConstant @ByteString () []],
      -- findFirstSetBit [0x00, 0x00] => -1
      [mkConstant @ByteString () [0x00, 0x00]],
      -- findFirstSetBit [0x00, 0x02] => 1
      [mkConstant @ByteString () [0x00, 0x02]],
      -- findFirstSetBit [0xFF, 0xF2] => 1
      [mkConstant @ByteString () [0xFF, 0xF2]]
    ]
