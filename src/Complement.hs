{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Complement
  ( case1,
    case2,
    case3,
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

-- complementByteString [] => []
case1 :: Term TyName Name DefaultUni DefaultFun ()
case1 =
  mkIterAppNoAnn
    (builtin () PLC.ComplementByteString)
    [ mkConstant @ByteString () []
    ]

-- complementByteString [0x0F] => [0xF0]
case2 :: Term TyName Name DefaultUni DefaultFun ()
case2 =
  mkIterAppNoAnn
    (builtin () PLC.ComplementByteString)
    [ mkConstant @ByteString () [0x0F]
    ]

-- complementByteString [0x4F, 0xF4] => [0xB0, 0x0B]
case3 :: Term TyName Name DefaultUni DefaultFun ()
case3 =
  mkIterAppNoAnn
    (builtin () PLC.ComplementByteString)
    [ mkConstant @ByteString () [0xB0, 0x0B]
    ]
