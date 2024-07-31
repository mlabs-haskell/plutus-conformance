{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Xor (cases) where

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
    (mkIterAppNoAnn (builtin () PLC.XorByteString))
    [ -- xorByteString False [] [0xFF] => []
      [mkConstant @Bool () False, mkConstant @ByteString () [], mkConstant @ByteString () [0xFF]],
      -- xorByteString False [0xFF] [] => []
      [mkConstant @Bool () False, mkConstant @ByteString () [0xFF], mkConstant @ByteString () []],
      -- xorByteString False [0xFF] [0x00] => [0xFF]
      [mkConstant @Bool () False, mkConstant @ByteString () [0xFF], mkConstant @ByteString () [0x00]],
      -- xorByteString False [0x00] [0xFF] => [0xFF]
      [mkConstant @Bool () False, mkConstant @ByteString () [0x00], mkConstant @ByteString () [0xFF]],
      -- xorByteString False [0x4F, 0x00] [0xF4] => [0xBB]
      [mkConstant @Bool () False, mkConstant @ByteString () [0x4F, 0x00], mkConstant @ByteString () [0xF4]],
      -- xorByteString True [] [0xFF] => [0xFF]
      [mkConstant @Bool () True, mkConstant @ByteString () [], mkConstant @ByteString () [0xFF]],
      -- xorByteString True [0xFF] [] => [0xFF]
      [mkConstant @Bool () True, mkConstant @ByteString () [0xFF], mkConstant @ByteString () []],
      -- xorByteString True [0xFF] [0x00] => [0xFF]
      [mkConstant @Bool () True, mkConstant @ByteString () [0xFF], mkConstant @ByteString () [0x00]],
      -- xorByteString True [0x00] [0xFF] => [0xFF]
      [mkConstant @Bool () True, mkConstant @ByteString () [0x00], mkConstant @ByteString () [0xFF]],
      -- xorByteString True [0x4F, 0x00] [0xF4] => [0xBB, 0x00]
      [mkConstant @Bool () True, mkConstant @ByteString () [0x4F, 0x00], mkConstant @ByteString () [0xF4]]
    ]
