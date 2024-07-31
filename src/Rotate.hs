{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Rotate (cases) where

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
    (mkIterAppNoAnn (builtin () PLC.RotateByteString))
    [ -- rotateByteString [] 3 => []
      [mkConstant @ByteString () [], mkConstant @Integer () 3],
      -- rotateByteString [] (-1) => []
      [mkConstant @ByteString () [], mkConstant @Integer () (-1)],
      -- rotateByteString [0xEB, 0xFC] 5 => [0x7F, 0x9D]
      [mkConstant @ByteString () [0xEB, 0xFC], mkConstant @Integer () 5],
      -- rotateByteString [0xEB, 0xFC] (-5) => [0xE7, 0x5F]
      [mkConstant @ByteString () [0xEB, 0xFC], mkConstant @Integer () (-5)],
      -- rotateByteString [0xEB, 0xFC] 16 => [0xEB, 0xFC]
      [mkConstant @ByteString () [0xEB, 0xFC], mkConstant @Integer () 16],
      -- rotateByteString [0xEB, 0xFC] (-16) => [0xEB, 0xFC]
      [mkConstant @ByteString () [0xEB, 0xFC], mkConstant @Integer () (-16)],
      -- rotateByteString [0xEB, 0xFC] 21 =>[0x7F, 0x9D]
      [mkConstant @ByteString () [0xEB, 0xFC], mkConstant @Integer () 21],
      -- rotateByteString [0xEB, 0xFC] (-21) => [0xE7, 0x5F]
      [mkConstant @ByteString () [0xEB, 0xFC], mkConstant @Integer () (-21)]
    ]
