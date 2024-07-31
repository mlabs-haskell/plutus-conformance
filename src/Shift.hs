{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Shift (cases) where

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
    (mkIterAppNoAnn (builtin () PLC.ShiftByteString))
    [ -- shiftByteString [] 3 => []
      [mkConstant @ByteString () [], mkConstant @Integer () 3],
      -- shiftByteString [] (-3) => []
      [mkConstant @ByteString () [], mkConstant @Integer () (-3)],
      -- shiftByteString [0xEB, 0xFC] 5 => [0x7F, 0x80]
      [mkConstant @ByteString () [0xEB, 0xFC], mkConstant @Integer () 5],
      -- shiftByteString [0xEB, 0xFC] (-5) => [0x07, 0x5F]
      [mkConstant @ByteString () [0xEB, 0xFC], mkConstant @Integer () (-5)],
      -- shiftByteString [0xEB, 0xFC] 16 => [0x00, 0x00]
      [mkConstant @ByteString () [0xEB, 0xFC], mkConstant @Integer () 16],
      -- shiftByteString [0xEB, 0xFC] (-16) => [0x00, 0x00]
      [mkConstant @ByteString () [0xEB, 0xFC], mkConstant @Integer () (-16)]
    ]
