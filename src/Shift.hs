{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Shift
  ( case1,
    case2,
    case3,
    case4,
    case5,
    case6,
  )
where

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

-- shiftByteString [] 3 => []
case1 :: Term TyName Name DefaultUni DefaultFun ()
case1 =
  mkIterAppNoAnn
    (builtin () PLC.ShiftByteString)
    [ mkConstant @ByteString () [],
      mkConstant @Integer () 3
    ]

-- shiftByteString [] (-3) => []
case2 :: Term TyName Name DefaultUni DefaultFun ()
case2 =
  mkIterAppNoAnn
    (builtin () PLC.ShiftByteString)
    [ mkConstant @ByteString () [],
      mkConstant @Integer () (-3)
    ]

-- shiftByteString [0xEB, 0xFC] 5 => [0x7F, 0x80]
case3 :: Term TyName Name DefaultUni DefaultFun ()
case3 =
  mkIterAppNoAnn
    (builtin () PLC.ShiftByteString)
    [ mkConstant @ByteString () [0xEB, 0xFC],
      mkConstant @Integer () 5
    ]

-- shiftByteString [0xEB, 0xFC] (-5) => [0x07, 0x5F]
case4 :: Term TyName Name DefaultUni DefaultFun ()
case4 =
  mkIterAppNoAnn
    (builtin () PLC.ShiftByteString)
    [ mkConstant @ByteString () [0xEB, 0xFC],
      mkConstant @Integer () (-5)
    ]

-- shiftByteString [0xEB, 0xFC] 16 => [0x00, 0x00]
case5 :: Term TyName Name DefaultUni DefaultFun ()
case5 =
  mkIterAppNoAnn
    (builtin () PLC.ShiftByteString)
    [ mkConstant @ByteString () [0xEB, 0xFC],
      mkConstant @Integer () 16
    ]

-- shiftByteString [0xEB, 0xFC] (-16) => [0x00, 0x00]
case6 :: Term TyName Name DefaultUni DefaultFun ()
case6 =
  mkIterAppNoAnn
    (builtin () PLC.ShiftByteString)
    [ mkConstant @ByteString () [0xEB, 0xFC],
      mkConstant @Integer () (-16)
    ]
