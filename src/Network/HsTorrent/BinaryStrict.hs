{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Network.HsTorrent.BinaryStrict
-- Copyright   :  AUTHORS
-- License     :  BSD3
-- Maintainer  :  https://github.com/hstorrent
-- Stability   :  experimental
-- Portability :  portable
--
-- This module has a single purpose, to expose 'BinaryS'.

module Network.HsTorrent.BinaryStrict where

import           Data.Binary
import           Data.ByteString
import qualified Data.ByteString.Lazy as BL

-- | This serves merely as a convenience class, providing wrappers to
-- work over strict 'ByteString's rather than lazy ones.
class Binary a ⇒ BinaryS a where
  -- | Just like 'encode' but calls 'BL.toStrict' on the result.
  encodeStrict ∷ a → ByteString
  encodeStrict = BL.toStrict . encode

  -- | Just like 'decode' but calls 'BL.fromStrict' first.
  decodeStrict ∷ ByteString → a
  decodeStrict = decode . BL.fromStrict
