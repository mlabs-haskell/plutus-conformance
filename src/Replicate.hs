{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Replicate (cases) where

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
    (mkIterAppNoAnn (builtin () PLC.ReplicateByte))
    [ -- replicateByte (-1) 0 => error
      [mkConstant @Integer () (-1), mkConstant @Integer () 0x00],
      -- replicateByte (-1) 3 => error
      [mkConstant @Integer () (-1), mkConstant @Integer () 0x03],
      -- replicateByte 1 (-1) => error
      [mkConstant @Integer () 1, mkConstant @Integer () (-1)],
      -- replicateByte 1 256 => error
      [mkConstant @Integer () 1, mkConstant @Integer () 256],
      -- replicateByte 4 (-1) => error
      [mkConstant @Integer () 4, mkConstant @Integer () (-1)],
      -- replicateByte 4 256 => error
      [mkConstant @Integer () 4, mkConstant @Integer () 256],
      -- replicateByte 0 0xFF => []
      [mkConstant @Integer () 0, mkConstant @Integer () 0xFF],
      -- replicateByte 4 0xFF => [0xFF, 0xFF, 0xFF, 0xFF]
      [mkConstant @Integer () 4, mkConstant @Integer () 0xFF]
    ]
