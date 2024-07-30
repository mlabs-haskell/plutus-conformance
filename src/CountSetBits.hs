{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module CountSetBits
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

-- countSetBits [] => 0
case1 :: Term TyName Name DefaultUni DefaultFun ()
case1 =
  mkIterAppNoAnn
    (builtin () PLC.CountSetBits)
    [ mkConstant @ByteString () []
    ]

-- countSetBits [0x00, 0x00] => 0
case2 :: Term TyName Name DefaultUni DefaultFun ()
case2 =
  mkIterAppNoAnn
    (builtin () PLC.CountSetBits)
    [ mkConstant @ByteString () [0x00, 0x00]
    ]

-- countSetBits [0x01, 0x00] => 1
case3 :: Term TyName Name DefaultUni DefaultFun ()
case3 =
  mkIterAppNoAnn
    (builtin () PLC.CountSetBits)
    [ mkConstant @ByteString () [0x01, 0x00]
    ]

-- countSetBits [0x00, 0x01] => 1
case4 :: Term TyName Name DefaultUni DefaultFun ()
case4 =
  mkIterAppNoAnn
    (builtin () PLC.CountSetBits)
    [ mkConstant @ByteString () [0x00, 0x01]
    ]
