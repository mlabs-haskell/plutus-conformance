{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Replicate
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

-- replicateByte (-1) 0 => error
case1 :: Term TyName Name DefaultUni DefaultFun ()
case1 =
  mkIterAppNoAnn
    (builtin () PLC.ReplicateByte)
    [ mkConstant @Integer () (-1),
      mkConstant @Integer () 0x00
    ]

-- replicateByte (-1) 3 => error
case2 :: Term TyName Name DefaultUni DefaultFun ()
case2 =
  mkIterAppNoAnn
    (builtin () PLC.ReplicateByte)
    [ mkConstant @Integer () (-1),
      mkConstant @Integer () 0x03
    ]

-- replicateByte 1 (-1) => error
case3 :: Term TyName Name DefaultUni DefaultFun ()
case3 =
  mkIterAppNoAnn
    (builtin () PLC.ReplicateByte)
    [ mkConstant @Integer () 1,
      mkConstant @Integer () (-1)
    ]

-- replicateByte 1 256 => error
case4 :: Term TyName Name DefaultUni DefaultFun ()
case4 =
  mkIterAppNoAnn
    (builtin () PLC.ReplicateByte)
    [ mkConstant @Integer () 1,
      mkConstant @Integer () 256
    ]

-- replicateByte 4 (-1) => error
case5 :: Term TyName Name DefaultUni DefaultFun ()
case5 =
  mkIterAppNoAnn
    (builtin () PLC.ReplicateByte)
    [ mkConstant @Integer () 4,
      mkConstant @Integer () (-1)
    ]

-- replicateByte 4 256 => error
case6 :: Term TyName Name DefaultUni DefaultFun ()
case6 =
  mkIterAppNoAnn
    (builtin () PLC.ReplicateByte)
    [ mkConstant @Integer () 4,
      mkConstant @Integer () 256
    ]

-- replicateByte 0 0xFF => []
case7 :: Term TyName Name DefaultUni DefaultFun ()
case7 =
  mkIterAppNoAnn
    (builtin () PLC.ReplicateByte)
    [ mkConstant @Integer () 0,
      mkConstant @Integer () 0xFF
    ]

-- replicateByte 4 0xFF => [0xFF, 0xFF, 0xFF, 0xFF]
case8 :: Term TyName Name DefaultUni DefaultFun ()
case8 =
  mkIterAppNoAnn
    (builtin () PLC.ReplicateByte)
    [ mkConstant @Integer () 4,
      mkConstant @Integer () 0xFF
    ]
