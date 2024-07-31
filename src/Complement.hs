{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Complement (cases) where

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
    (mkIterAppNoAnn (builtin () PLC.ComplementByteString))
    [ -- complementByteString [] => []
      [mkConstant @ByteString () []],
      -- complementByteString [0x0F] => [0xF0]
      [mkConstant @ByteString () [0x0F]],
      -- complementByteString [0x4F, 0xF4] => [0xB0, 0x0B]
      [mkConstant @ByteString () [0xB0, 0x0B]]
    ]
