{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Network.HsTorrent.Handshake
-- Copyright   :  AUTHORS
-- License     :  BSD3
-- Maintainer  :  https://github.com/hstorrent
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation of handshake as described in the Peer wire protocol at
-- https://wiki.theory.org/BitTorrentSpecification#Peer_wire_protocol_.28TCP.29

module Network.HsTorrent.Handshake where

import Control.Lens
import Control.Applicative
import Control.Monad
import Data.Binary
import Data.ByteString (ByteString, length, pack, unpack)
import Data.Char (ord)
import Data.Default (Default(..))
import Network.HsTorrent.BinaryStrict

-- * Types

-- | Wrapper for eight 'Word8's.
newtype EightBytes = EightBytes { _unEight ∷ ( Word8, Word8, Word8, Word8
                                              , Word8, Word8, Word8, Word8
                                              )
                                } deriving (Show, Eq)

-- | Wrapper for twenty 'Word8's.
newtype TwentyBytes = TwentyBytes { _unTwenty ∷ ( EightBytes
                                                 , EightBytes
                                                 , Word8, Word8, Word8, Word8
                                                 )
                                  } deriving (Show, Eq)

-- | 8 reserved bytes
newtype Reserved = Reserved { _reserved ∷ EightBytes }
                 deriving (Show, Eq)

-- | 20-byte SHA1 hash of the info key
newtype InfoHash = InfoHash { _infoHash ∷ TwentyBytes }
                 deriving (Show, Eq)

-- | Peer ID identifies the client version used by the peer.
newtype PeerId = PeerId { _peerId ∷ TwentyBytes }
               deriving (Show, Eq)

-- | Protocol string
newtype Protocol = Protocol { _protocol ∷ ByteString }
                 deriving (Show, Eq)


-- | The handshake is a required message and must be the first message
-- transmitted by the client. It is (49 + len(pstr)) bytes long.
--
-- * 1 byte for pstrlen
-- * @length pstr@ bytes for pstr
-- * 8 bytes reserved
-- * 20 bytes for info_hash
-- * 20 bytes for peer_id
--
-- > handshake: <pstrlen><pstr><reserved><info_hash><peer_id>
--
-- We don't store the length of @pstr@ ourselves because we can figure
-- it out on the fly and it ensures nothing funky like overwriting the
-- length ever happens. The 'Binary' instance handles sending and
-- decoding the length.
data Handshake = Handshake
  { _hPstr ∷ Protocol
  , _hReserved ∷ Reserved
  , _hInfoHash ∷ InfoHash
  , _hPeerId ∷ PeerId
  } deriving (Show, Eq)

-- | Creates a 'Handshake' from only 'InfoHash'. Uses the 'Default'
-- instance for 'PeerId'.
mkHandshake ∷ InfoHash → Handshake
mkHandshake i = Handshake { _hPstr = def
                          , _hReserved = def
                          , _hInfoHash = i
                          , _hPeerId = def
                          }

-- * Instances

-- | 'put's a 'ByteString' by writing its length first as (truncated)
-- 'Word8' followed by the unpacked bytes directly.
--
-- Differs from the default instance in that it doesn't stick extra
-- gunk in front.
putBS ∷ ByteString → Put
putBS b = do
  put . (fromIntegral ∷ Int → Word8) $ Data.ByteString.length b
  mapM_ put . unpack $ b

-- | Dual of 'putBS'.
getBS ∷ Get ByteString
getBS = do
  (l ∷ Word8) ← get
  pack <$> replicateM (fromIntegral l) get

-- | Turns a 'Char' into 'Word8'. If the char is outside the Word8, it
-- gets truncated.
toByte ∷ Char → Word8
toByte = fromIntegral . ord

-- | Zero'd out 8 bytes.
empty8Bytes ∷ EightBytes
empty8Bytes = EightBytes (0, 0, 0, 0, 0, 0, 0, 0)

-- | Zeroed out 20 bytes.
empty20Bytes ∷ TwentyBytes
empty20Bytes = TwentyBytes (empty8Bytes, empty8Bytes, 0, 0, 0, 0)

instance Default Reserved where
  def = Reserved empty8Bytes

instance Default Protocol where
  def = Protocol . pack $ map toByte "BitTorrent protocol"

-- | Azureus style.
instance Default PeerId where
  def = PeerId pid
    where
      ver ∷ EightBytes
      ver = EightBytes (toByte '-',
                        toByte 'H', toByte 'S',
                        toByte '0', toByte '0', toByte '0', toByte '0',
                        toByte '-')

      pid ∷ TwentyBytes
      pid = TwentyBytes (ver, empty8Bytes, 0, 0, 0, 0)

instance Binary Protocol where
  put = putBS . _protocol
  get = Protocol <$> getBS

instance BinaryS Protocol where

instance Binary EightBytes where
  put (EightBytes b) = put b
  get = EightBytes <$> get

instance BinaryS EightBytes where

instance Binary TwentyBytes where
  put (TwentyBytes b) = put b
  get = do
    (b, b') ← (,) <$> get <*> get
    (x, y, z, w) ← liftM4 (,,,) get get get get
    return $ TwentyBytes (b, b', x, y, z, w)

instance BinaryS TwentyBytes where

instance Binary Reserved where
  put = put . _reserved
  get = Reserved <$> get

instance BinaryS Reserved where

instance Binary InfoHash where
  put = put . _infoHash
  get = InfoHash <$> get

instance BinaryS InfoHash where

instance Binary PeerId where
  put = put . _peerId
  get = PeerId <$> get

instance BinaryS PeerId where

instance Binary Handshake where
  put Handshake {..} = put (_hPstr, _hReserved, _hInfoHash, _hPeerId)
  get = liftM4 Handshake get get get get

instance BinaryS Handshake where

-- * Lenses
makeLenses ''Handshake
makeLenses ''InfoHash
makeLenses ''PeerId
makeLenses ''Reserved
makeLenses ''Protocol
