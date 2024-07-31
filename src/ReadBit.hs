{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module ReadBit (cases) where

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
    (mkIterAppNoAnn (builtin () PLC.ReadBit))
    [ -- readBit [] 0 => error
      [mkConstant @ByteString () [], mkConstant @Integer () 0],
      -- readBit [] 345 => error
      [mkConstant @ByteString () [], mkConstant @Integer () 345],
      -- readBit [] (-1) => error
      [mkConstant @ByteString () [], mkConstant @Integer () (-1)],
      -- readBit [0xFF] (-1) => error
      [mkConstant @ByteString () [0xFF], mkConstant @Integer () (-1)],
      -- readBit [0xF4] 0 => False
      [mkConstant @ByteString () [0xF4], mkConstant @Integer () 0],
      -- readBit [0xF4] 1 => False
      [mkConstant @ByteString () [0xF4], mkConstant @Integer () 1],
      -- readBit [0xF4] 2 => True
      [mkConstant @ByteString () [0xF4], mkConstant @Integer () 2],
      -- readBit [0xF4] 3 => False
      [mkConstant @ByteString () [0xF4], mkConstant @Integer () 3],
      -- readBit [0xF4] 4 => True
      [mkConstant @ByteString () [0xF4], mkConstant @Integer () 4],
      -- readBit [0xF4] 5 => True
      [mkConstant @ByteString () [0xF4], mkConstant @Integer () 5],
      -- readBit [0xF4] 6 => True
      [mkConstant @ByteString () [0xF4], mkConstant @Integer () 6],
      -- readBit [0xF4] 7 => True
      [mkConstant @ByteString () [0xF4], mkConstant @Integer () 7],
      -- readBit [0xF4] 8 => error
      [mkConstant @ByteString () [0xF4], mkConstant @Integer () 8],
      -- readBit [0xFF, 0xF4] 16 => error
      [mkConstant @ByteString () [0xFF, 0xF4], mkConstant @Integer () 16],
      -- readBit [0xF4, 0xFF] 10 => False
      [mkConstant @ByteString () [0xF4, 0xFF], mkConstant @Integer () 10]
    ]
