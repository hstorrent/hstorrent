{-# LANGUAGE CPP           #-}
{-# LANGUAGE TupleSections #-}

module Network.HsTorrent.TorrentParserSpec where

import           Network.HsTorrent.TorrentParser

import           Data.BEncode                    as BE
import qualified Data.ByteString                 as B
import           System.Directory                (getDirectoryContents)
import           System.FilePath                 (takeExtension, (</>))
import           Test.Hspec
import           Test.Hspec.HUnit
import           Test.Hspec.QuickCheck
import           Test.HUnit

#if MIN_VERSION_base(4,7,0)
import           Data.Either                     (isLeft, isRight)
#else
isLeft :: Either a b -> Bool
isLeft Left{} = True
isLeft _      = False

isRight :: Either a b -> Bool
isRight Right{} = True
isRight _       = False
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parsing" $ do
    fromHUnitTest $ TestLabel "should parse" shouldParse
    fromHUnitTest $ TestLabel "should not parse" shouldNotParse

  describe "parsing-printing" $ do
    prop "forall d, fromBEncode . toBEncode $ d = Right d" $ \d ->
      (BE.fromBEncode . BE.toBEncode) (d :: TorrentMetaInfo) `shouldBe` (Right d)

shouldParse :: Test
shouldParse = TestCase $ do
  tfs <- getTorrentFiles "tests/should_parse"
  sequence_ $ flip map tfs $ \(f, c) ->
    assertBool ("Can't parse torrent file: " ++ f) $ isRight (decode c :: Result TorrentMetaInfo)

shouldNotParse :: Test
shouldNotParse = TestCase $ do
  tfs <- getTorrentFiles "tests/should_not_parse"
  sequence_ $ flip map tfs $ \(f, c) ->
    assertBool ("Parsed a broken torrent file: " ++ f) $ isLeft (decode c :: Result TorrentMetaInfo)

getTorrentFiles :: FilePath -> IO [(FilePath, B.ByteString)]
getTorrentFiles root =
  mapM (\p -> (p,) `fmap` B.readFile p) . filter ((==) ".torrent" . takeExtension) . map (root </>)
    =<< getDirectoryContents root

