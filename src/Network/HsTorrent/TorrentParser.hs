{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Network.HsTorrent.TorrentParser
-- Copyright   :  AUTHORS
-- License     :  BSD3
-- Maintainer  :  https://github.com/hstorrent
-- Stability   :  experimental
-- Portability :  portable
--
-- .torrent file structure and parser.

module Network.HsTorrent.TorrentParser where


import           Control.Applicative
import           Control.Lens.TH
import           Control.Monad
import           Data.BEncode              as BE
import qualified Data.BEncode.BDict        as BE
import qualified Data.ByteString           as B
import           Data.Typeable
import           GHC.Generics
import           Test.QuickCheck           hiding (Result)
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Instances


-- | Metainfo file as specified in
-- https://wiki.theory.org/BitTorrentSpecification#Metainfo_File_Structure
data TorrentMetaInfo = TorrentMetaInfo
  { _tAnnounce     :: B.ByteString
  , _tAnnounceList :: Maybe [[B.ByteString]]
  , _tCreationDate :: Maybe Int
  , _tComment      :: Maybe B.ByteString
  , _tCreatedBy    :: Maybe B.ByteString
  , _tEncoding     :: Maybe B.ByteString
  , _tInfo         :: Info
  } deriving (Show, Eq, Typeable, Generic)

data Info = Info
  { _iName        :: B.ByteString
  , _iPieceLength :: Int
  , _iPieces      :: [B.ByteString]
  , _iPrivate     :: Bool
  , _iFiles       :: [File]
  } deriving (Show, Eq, Typeable, Generic)

data File = File
  { _fLength :: Int
  , _fMd5Sum :: Maybe B.ByteString
  , _fPath   :: [B.ByteString]
  } deriving (Show, Eq, Typeable, Generic)


-- | Parse contents of a .torrent file.
parseTorrentMetaInfo :: B.ByteString -> Result TorrentMetaInfo
parseTorrentMetaInfo = decode


-- * BEncode instances

instance BEncode TorrentMetaInfo where
  toBEncode TorrentMetaInfo{..} = toDict $
       "announce"      .=! _tAnnounce
    .: "announce-list" .=? _tAnnounceList
    .: "creation date" .=? _tCreationDate
    .: "comment"       .=? _tComment
    .: "created by"    .=? _tCreatedBy
    .: "encoding"      .=? _tEncoding
    .: "info"          .=! _tInfo
    .: endDict

  fromBEncode = fromDict $
    TorrentMetaInfo <$>! "announce"
                    <*>? "announce-list"
                    <*>? "creation date"
                    <*>? "comment"
                    <*>? "created by"
                    <*>? "encoding"
                    <*>! "info"

instance BEncode Info where
  toBEncode i = toDict $
       "files"        .=! _iFiles i
    .: "name"         .=! _iName i
    .: "piece length" .=! _iPieceLength i
    .: "pieces"       .=! B.concat (_iPieces i)
    .: "private"      .=? (if _iPrivate i then Just True else Nothing)
    .: endDict

  fromBEncode bv@(BDict (BE.Cons key _ _))
    | key == "length" = flip fromDict bv $ do
        -- first key is "length", single file mode
        flen <- field $ req "length"
        md5sum <- optional $ field $ req "md5sum"
        name <- field $ req "name"
        pieceLength <- field $ req "piece length"
        pieces <- splitPieces <$> (field $ req "pieces")
        private <- readPrivate
        return $ Info name pieceLength pieces private [File flen md5sum []]
    | otherwise = flip fromDict bv $ do
        -- multi file mode
        files <- field $ req "files"
        name <- field $ req "name"
        pieceLength <- field $ req "piece length"
        pieces <- splitPieces <$> (field $ req "pieces")
        private <- readPrivate
        return $ Info name pieceLength pieces private files
    where
      readPrivate :: Get Bool
      readPrivate = maybe False id <$> (optional $ field $ req "private")

      readPieces :: Get [B.ByteString]
      readPieces = do
        bs <- field $ req "pieces"
        return $ splitPieces bs

      splitPieces :: B.ByteString -> [B.ByteString]
      splitPieces bs
        | B.length bs < 20 = []
        | otherwise = let (h, t) = B.splitAt 20 bs in h : splitPieces t

instance BEncode File where
  toBEncode File{..} = toDict $
       "length" .=! _fLength
    .: "md5sum" .=? _fMd5Sum
    .: "path"   .=! _fPath
    .: endDict

  fromBEncode = fromDict $ File <$>! "length" <*>? "md5sum" <*>! "path"


-- * Arbitrary instances

instance Arbitrary TorrentMetaInfo where
  arbitrary =
    liftM5 TorrentMetaInfo arbitrary arbitrary arbitrary arbitrary arbitrary
      <*> arbitrary <*> arbitrary
  shrink = recursivelyShrink

instance Arbitrary Info where
  arbitrary = liftM5 Info arbitrary arbitrary (listOf (B.pack <$> vector 20)) arbitrary arbitrary
  shrink = recursivelyShrink

instance Arbitrary File where
  arbitrary = File <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = recursivelyShrink


-- * Lenses

makeLenses ''TorrentMetaInfo
makeLenses ''Info
makeLenses ''File

