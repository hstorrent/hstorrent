{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Network.HsTorrent.Message
-- Copyright   :  AUTHORS
-- License     :  BSD3
-- Maintainer  :  https://github.com/hstorrent
-- Stability   :  experimental
-- Portability :  portable
--
-- Types and serialisation for
-- https://wiki.theory.org/BitTorrentSpecification#Messages

module Network.HsTorrent.Message where

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Put
import Data.ByteString hiding (concat)
import Network.HsTorrent.BinaryStrict
import Prelude hiding (length)

-- | zero-based piece index
type PieceIndex = Word32

-- | zero-based byte offset of the piece
type ByteOffset = Word32

-- | Used by request and cancel messages.
type Length = Word32

-- | Encoding of all the messages we might exchange. All the length
-- and message ID handling is done in (de)serialisation so any user of
-- this type does not have to worry about those.
data Message = KeepAlive | Choke | Unchoke | Interested | NotInterested
             | Have PieceIndex
             | Bitfield ByteString
             | Request PieceIndex ByteOffset Length
             | Piece PieceIndex ByteOffset ByteString
             | Cancel PieceIndex ByteOffset Length
             | Port Word16
             deriving (Show, Eq)

-- | Each message except 'KeepAlive' has its own ID. 'KeepAlive' is
-- the special, 0-length message. The general format is
--
-- > <length><id><payload>
--
-- length = Word32, id = Word8 and payload is either a series of
-- bytes ('ByteString' here) or a preset number of 'Word32's.
--
-- Only 'Bitfield' and 'Piece' have a varying length, we know the
-- length of all other messages ahead of time.
instance Binary Message where
  put KeepAlive       = putWord32be 0
  put Choke           = putWord32be 1 >> putWord8 0
  put Unchoke         = putWord32be 1 >> putWord8 1
  put Interested      = putWord32be 1 >> putWord8 2
  put NotInterested   = putWord32be 1 >> putWord8 3
  put (Have x)        = putWord32be 5 >> putWord8 4 >> put x
  put (Bitfield x)    = let l = fromIntegral (length x) + 1
                        in putWord32be l >> putWord8 5 >> putByteString x
  put (Request i b l) = putWord32be 13 >> putWord8 6 >> put i >> put b >> put l
  put (Piece i b b')  = let l = fromIntegral (length b') + 9
                        in putWord32be l >> putWord8 7
                           >> put i >> put b >> putByteString b'
  put (Cancel i b l)  = putWord32be 13 >> putWord8 8 >> put i >> put b >> put l
  put (Port p)        = putWord32be 3 >> putWord8 9 >> put p

  get = (get ∷ Get Word32) >>= \case
    0 → return KeepAlive
    n → (n,) <$> (get ∷ Get Word8) >>= \case
      (1,  0) → return Choke
      (1,  1) → return Unchoke
      (1,  2) → return Interested
      (1,  3) → return NotInterested
      (5,  4) → Have <$> get
      (l,  5) | l >= 1 → let x = fromIntegral $ l - 1
                         in Bitfield . pack <$> replicateM x get
      (13, 6) → liftA3 Request get get get
      (l,  7) | l >= 9 → let x = fromIntegral $ l - 9
                         in liftA3 Piece get get (pack <$> replicateM x get)
      (13, 8) → liftA3 Cancel get get get
      (3,  9) → Port <$> get
      (l,  i) → fail $ concat [ "Unknown message: <len=", show l
                              , "><id=", show i, ">"
                              ]

instance BinaryS Message where
