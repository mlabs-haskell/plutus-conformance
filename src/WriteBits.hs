{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module WriteBits (cases) where

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
    (mkIterAppNoAnn (builtin () PLC.WriteBits))
    [ -- writeBits [] [0] [False] => error
      [mkConstant @ByteString () [], mkConstant @[Integer] () [0], mkConstant @[Bool] () [False]],
      -- writeBits [] [15] [False] => error
      [mkConstant @ByteString () [], mkConstant @[Integer] () [15], mkConstant @[Bool] () [False]],
      -- writeBits [] [0] [True] => error
      [mkConstant @ByteString () [], mkConstant @[Integer] () [0], mkConstant @[Bool] () [True]],
      -- writeBits [] [0, 1] [False, False] => error
      [mkConstant @ByteString () [], mkConstant @[Integer] () [0, 1], mkConstant @[Bool] () [False, False]],
      -- writeBits [0xFF] [(-1)] [False] => error
      [mkConstant @ByteString () [0xFF], mkConstant @[Integer] () [(-1)], mkConstant @[Bool] () [False]],
      -- writeBits [0xFF] [0, (-1)] [False, False] => error
      [mkConstant @ByteString () [0xFF], mkConstant @[Integer] () [0, (-1)], mkConstant @[Bool] () [False, False]],
      -- writeBits [0xFF] [(-1), 0] [False, False] => error
      [mkConstant @ByteString () [0xFF], mkConstant @[Integer] () [(-1), 0], mkConstant @[Bool] () [False, False]],
      -- writeBits [0xFF] [8] [False] => error
      [mkConstant @ByteString () [0xFF], mkConstant @[Integer] () [8], mkConstant @[Bool] () [False]],
      -- writeBits [0xFF] [1, 8] [False, False] => error
      [mkConstant @ByteString () [0xFF], mkConstant @[Integer] () [1, 8], mkConstant @[Bool] () [False, False]],
      -- writeBits [0xFF] [8, 1] [False, False] => error
      [mkConstant @ByteString () [0xFF], mkConstant @[Integer] () [8, 1], mkConstant @[Bool] () [False, False]],
      -- writeBits [0xFF] [0] [False] => [0xFE]
      [mkConstant @ByteString () [0xFF], mkConstant @[Integer] () [0], mkConstant @[Bool] () [False]],
      -- writeBits [0xFF] [1] [False] => [0xFD]
      [mkConstant @ByteString () [0xFF], mkConstant @[Integer] () [1], mkConstant @[Bool] () [False]],
      -- writeBits [0xFF] [2] [False] => [0xFB]
      [mkConstant @ByteString () [0xFF], mkConstant @[Integer] () [2], mkConstant @[Bool] () [False]],
      -- writeBits [0xFF] [3] [False] => [0xF7]
      [mkConstant @ByteString () [0xFF], mkConstant @[Integer] () [3], mkConstant @[Bool] () [False]],
      -- writeBits [0xFF] [4] [False] => [0xEF]
      [mkConstant @ByteString () [0xFF], mkConstant @[Integer] () [4], mkConstant @[Bool] () [False]],
      -- writeBits [0xFF] [5] [False] => [0xDF]
      [mkConstant @ByteString () [0xFF], mkConstant @[Integer] () [5], mkConstant @[Bool] () [False]],
      -- writeBits [0xFF] [6] [False] => [0xBF]
      [mkConstant @ByteString () [0xFF], mkConstant @[Integer] () [6], mkConstant @[Bool] () [False]],
      -- writeBits [0xFF] [7] [False] => [0x7F]
      [mkConstant @ByteString () [0xFF], mkConstant @[Integer] () [7], mkConstant @[Bool] () [False]],
      -- writeBits [0x00] [5] [True] => [0x20]
      [mkConstant @ByteString () [0x00], mkConstant @[Integer] () [5], mkConstant @[Bool] () [True]],
      -- writeBits [0xFF] [5] [False] => [0xDF]
      [mkConstant @ByteString () [0xFF], mkConstant @[Integer] () [5], mkConstant @[Bool] () [False]],
      -- writeBits [0xF4, 0xFF] [10] [False] => [0xF0, 0xFF]
      [mkConstant @ByteString () [0xF4, 0xFF], mkConstant @[Integer] () [10], mkConstant @[Bool] () [False]],
      -- writeBits [0xF4, 0xFF] [10, 1] [False, False] => [0xF0, 0xFD]
      [mkConstant @ByteString () [0xF4, 0xFF], mkConstant @[Integer] () [10, 1], mkConstant @[Bool] () [False, False]],
      -- writeBits [0xF4, 0xFF] [10] [False, False] => [0xF0, 0xFF]
      [mkConstant @ByteString () [0xF4, 0xFF], mkConstant @[Integer] () [10], mkConstant @[Bool] () [False, False]],
      -- writeBits [0xF4, 0xFF] [10, 1] [False] => [0xF0, 0xFF]
      [mkConstant @ByteString () [0xF4, 0xFF], mkConstant @[Integer] () [10, 1], mkConstant @[Bool] () [False]],
      -- writeBits [0xF4, 0xFF] [1, 10] [False, False] => [0xF0, 0xFD]
      [mkConstant @ByteString () [0xF4, 0xFF], mkConstant @[Integer] () [1, 10], mkConstant @[Bool] () [False, False]],
      -- writeBits [0x00, 0xFF] [10, 10] [True, False] => [0x00, 0xFF]
      [mkConstant @ByteString () [0x00, 0xFF], mkConstant @[Integer] () [10, 10], mkConstant @[Bool] () [True, False]],
      -- writeBits [0x00, 0xFF] [10, 10] [False, True] => [0x04, 0xFF]
      [mkConstant @ByteString () [0x00, 0xFF], mkConstant @[Integer] () [10, 10], mkConstant @[Bool] () [False, True]],
      -- writeBits [0xFF] [0] [True] => [0xFF]
      [mkConstant @ByteString () [0xFF], mkConstant @[Integer] () [0], mkConstant @[Bool] () [True]],
      -- writeBits [0x00] [0] [False] => [0x00]
      [mkConstant @ByteString () [0x00], mkConstant @[Integer] () [0], mkConstant @[Bool] () [False]]
    ]
