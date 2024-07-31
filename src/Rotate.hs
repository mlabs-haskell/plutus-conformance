{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Rotate
  ( case1,
    case2,
    case3,
    case4,
    case5,
    case6,
    case7,
    case8,
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

-- rotateByteString [] 3 => []
case1 :: Term TyName Name DefaultUni DefaultFun ()
case1 =
  mkIterAppNoAnn
    (builtin () PLC.RotateByteString)
    [ mkConstant @ByteString () [],
      mkConstant @Integer () 3
    ]

-- rotateByteString [] (-1) => []
case2 :: Term TyName Name DefaultUni DefaultFun ()
case2 =
  mkIterAppNoAnn
    (builtin () PLC.RotateByteString)
    [ mkConstant @ByteString () [],
      mkConstant @Integer () (-1)
    ]

-- rotateByteString [0xEB, 0xFC] 5 => [0x7F, 0x9D]
case3 :: Term TyName Name DefaultUni DefaultFun ()
case3 =
  mkIterAppNoAnn
    (builtin () PLC.RotateByteString)
    [ mkConstant @ByteString () [0xEB, 0xFC],
      mkConstant @Integer () 5
    ]

-- rotateByteString [0xEB, 0xFC] (-5) => [0xE7, 0x5F]
case4 :: Term TyName Name DefaultUni DefaultFun ()
case4 =
  mkIterAppNoAnn
    (builtin () PLC.RotateByteString)
    [ mkConstant @ByteString () [0xEB, 0xFC],
      mkConstant @Integer () (-5)
    ]

-- rotateByteString [0xEB, 0xFC] 16 => [0xEB, 0xFC]
case5 :: Term TyName Name DefaultUni DefaultFun ()
case5 =
  mkIterAppNoAnn
    (builtin () PLC.RotateByteString)
    [ mkConstant @ByteString () [0xEB, 0xFC],
      mkConstant @Integer () 16
    ]

-- rotateByteString [0xEB, 0xFC] (-16) => [0xEB, 0xFC]
case6 :: Term TyName Name DefaultUni DefaultFun ()
case6 =
  mkIterAppNoAnn
    (builtin () PLC.RotateByteString)
    [ mkConstant @ByteString () [0xEB, 0xFC],
      mkConstant @Integer () (-16)
    ]

-- rotateByteString [0xEB, 0xFC] 21 =>[0x7F, 0x9D]
case7 :: Term TyName Name DefaultUni DefaultFun ()
case7 =
  mkIterAppNoAnn
    (builtin () PLC.RotateByteString)
    [ mkConstant @ByteString () [0xEB, 0xFC],
      mkConstant @Integer () 21
    ]

-- rotateByteString [0xEB, 0xFC] (-21) => [0xE7, 0x5F]
case8 :: Term TyName Name DefaultUni DefaultFun ()
case8 =
  mkIterAppNoAnn
    (builtin () PLC.RotateByteString)
    [ mkConstant @ByteString () [0xEB, 0xFC],
      mkConstant @Integer () (-21)
    ]
