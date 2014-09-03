{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HsTorrent.HandshakeSpec (main, spec) where

import           Control.Applicative
import           Control.Lens hiding (re)
import           Data.Binary
import qualified Data.ByteString as BS
import           Network.HsTorrent.BinaryStrict
import           Network.HsTorrent.Handshake
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck (Arbitrary(..))
import           Test.QuickCheck.Instances ()

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec = do
  describe "Handshake serialisation" $ do
    describe "Protocol" $ do
      -- We can encode and decode without loss of information
      prop "decode . encode ≡ id" $ \(Pr p) →
        (decode . encode) p `shouldBe` p

      -- The encoded length is exactly 1 byte longer than the inner BS
      prop "BS.length . encodeStrict ≡ succ . BS.length . _protocol" $
        \(Pr p) → (BS.length . encodeStrict) p
                  `shouldBe` (succ . BS.length . _protocol) p

    describe "EightBytes" $ do
      prop "decode . encode ≡ id" $ \(Ei p) →
        (decode . encode) p `shouldBe` p

      -- Takes precisely 8 bytes to encode
      prop "(BS.length . encodeStrict) x ≡ 8" $ \(Ei p) →
        (BS.length . encodeStrict) p `shouldBe` 8

    describe "TwentyBytes" $ do
      prop "decode . encode ≡ id" $ \(Ei p) →
        (decode . encode) p `shouldBe` p

      prop "(BS.length . encodeStrict) x ≡ 20" $ \(Tw p) →
        (BS.length . encodeStrict) p `shouldBe` 20

    describe "Reserved" $ do
      prop "decode . encode ≡ id" $ \(Re p) →
        (decode . encode) p `shouldBe` p

      prop "(BS.length . encodeStrict) x ≡ 8" $ \(Re p) →
        (BS.length . encodeStrict) p `shouldBe` 8

    describe "InfoHash" $ do
      prop "decode . encode ≡ id" $ \(In p) →
        (decode . encode) p `shouldBe` p

      prop "(BS.length . encodeStrict) x ≡ 20" $ \(In p) →
        (BS.length . encodeStrict) p `shouldBe` 20

    describe "PeerId" $ do
      prop "decode . encode ≡ id" $ \(Pe p) →
        (decode . encode) p `shouldBe` p

      prop "(BS.length . encodeStrict) x ≡ 20" $ \(Pe p) →
        (BS.length . encodeStrict) p `shouldBe` 20

    describe "Handshake" $ do
      prop "decode . encode ≡ id" $ \(Ha p) →
        (decode . encode) p `shouldBe` p

      prop "Encoded length is 49 + protocol string length" $ \(Ha p) (Pr p') →
        let h = p & hPstr .~ p'
            l = case p' of Protocol c → BS.length c
        in (BS.length . encodeStrict) h `shouldBe` 49 + l


newtype Pr = Pr { _unPr ∷ Protocol } deriving (Show, Eq)

instance Arbitrary Pr where
  arbitrary = Pr . Protocol <$> arbitrary

newtype Ei = Ei { _unEi ∷ EightBytes } deriving (Show, Eq)

instance Arbitrary Ei where
  arbitrary = do
    x ← (,,,,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
         <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    return . Ei $ EightBytes x

newtype Tw = Tw { _unTw ∷ TwentyBytes } deriving (Show, Eq)

instance Arbitrary Tw where
  arbitrary = do
    x ← (,,,,,) <$> (_unEi <$> arbitrary) <*> (_unEi <$> arbitrary)
         <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    return . Tw $ TwentyBytes x

newtype Re = Re { _unRe ∷ Reserved } deriving (Show, Eq)

instance Arbitrary Re where
  arbitrary = Re . Reserved . _unEi <$> arbitrary

newtype In = In { _unIn ∷ InfoHash } deriving (Show, Eq)

instance Arbitrary In where
  arbitrary = In . InfoHash . _unTw <$> arbitrary

newtype Pe = Pe { _unPe ∷ PeerId } deriving (Show, Eq)

instance Arbitrary Pe where
  arbitrary = Pe . PeerId . _unTw <$> arbitrary

newtype Ha = Ha { _unHa ∷ Handshake } deriving (Show, Eq)

instance Arbitrary Ha where
  arbitrary = do
    pr ← _unPr <$> arbitrary
    re ← _unRe <$> arbitrary
    inf ← _unIn <$> arbitrary
    pe ← _unPe <$> arbitrary
    return . Ha $ Handshake pr re inf pe
