{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module And (cases) where

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
    (mkIterAppNoAnn (builtin () PLC.AndByteString))
    [ -- andByteString False [] [0xFF] => []
      [mkConstant @Bool () False, mkConstant @ByteString () [], mkConstant @ByteString () [0xFF]],
      -- andByteString False [0xFF] [] => []
      [mkConstant @Bool () False, mkConstant @ByteString () [0xFF], mkConstant @ByteString () []],
      -- andByteString False [0xFF] [0x00] => [0x00]
      [mkConstant @Bool () False, mkConstant @ByteString () [0xFF], mkConstant @ByteString () [0x00]],
      -- andByteString False [0x00] [0xFF] => [0x00]
      [mkConstant @Bool () False, mkConstant @ByteString () [0x00], mkConstant @ByteString () [0xFF]],
      -- andByteString False [0x4F, 0x00] [0xF4] => [0x44]
      [mkConstant @Bool () False, mkConstant @ByteString () [0x4F, 0x00], mkConstant @ByteString () [0xF4]],
      -- andByteString True [] [0xFF] => [0xFF]
      [mkConstant @Bool () True, mkConstant @ByteString () [], mkConstant @ByteString () [0xFF]],
      -- andByteString True [0xFF] [] => [0xFF]
      [mkConstant @Bool () True, mkConstant @ByteString () [0xFF], mkConstant @ByteString () []],
      -- andByteString True [0xFF] [0x00] => [0x00]
      [mkConstant @Bool () True, mkConstant @ByteString () [0xFF], mkConstant @ByteString () [0x00]],
      -- andByteString True [0x00] [0xFF] => [0x00]
      [mkConstant @Bool () True, mkConstant @ByteString () [0x00], mkConstant @ByteString () [0xFF]],
      -- andByteString True [0x4F, 0x00] [0xF4] => [0x44, 0x00]
      [mkConstant @Bool () True, mkConstant @ByteString () [0x4F, 0x00], mkConstant @ByteString () [0xF4]]
    ]
