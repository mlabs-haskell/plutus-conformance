{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module FindFirstSetBit
  ( case1,
    case2,
    case3,
    case4,
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

-- findFirstSetBit [] => -1
case1 :: Term TyName Name DefaultUni DefaultFun ()
case1 =
  mkIterAppNoAnn
    (builtin () PLC.FindFirstSetBit)
    [ mkConstant @ByteString () []
    ]

-- findFirstSetBit [0x00, 0x00] => -1
case2 :: Term TyName Name DefaultUni DefaultFun ()
case2 =
  mkIterAppNoAnn
    (builtin () PLC.FindFirstSetBit)
    [ mkConstant @ByteString () [0x00, 0x00]
    ]

-- findFirstSetBit [0x00, 0x02] => 1
case3 :: Term TyName Name DefaultUni DefaultFun ()
case3 =
  mkIterAppNoAnn
    (builtin () PLC.FindFirstSetBit)
    [ mkConstant @ByteString () [0x00, 0x02]
    ]

-- findFirstSetBit [0xFF, 0xF2] => 1
case4 :: Term TyName Name DefaultUni DefaultFun ()
case4 =
  mkIterAppNoAnn
    (builtin () PLC.FindFirstSetBit)
    [ mkConstant @ByteString () [0xFF, 0xF2]
    ]