{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HsTorrent.MessageSpec (main, spec) where

import Control.Applicative
import Control.Monad (replicateM)
import Data.Binary
import Data.Binary.Get (runGet)
import Data.Binary.Put (putWord32be, runPut)
import Data.ByteString hiding (drop, take, length)
import Network.HsTorrent.BinaryStrict
import Network.HsTorrent.Message
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()


main ∷ IO ()
main = hspec spec

encodesTo ∷ BinaryS a ⇒ a → [Word8] → Expectation
x `encodesTo` bs = encodeStrict x `shouldBe` pack bs

hasId ∷ BinaryS a ⇒ a → Word8 → Expectation
x `hasId` i = case unpack $ encodeStrict x of
  _:_:_:_:i':_ → i' `shouldBe` i
  x' → fail $ "No ID found: " ++ show x'

spec ∷ Spec
spec = do
  describe "Message serialisation" $ do
    prop "decode . encode ≡ id" $ \(Me m) →
      (decode . encode) m `shouldBe` m

    describe "constant encodes" $ do
      it "KeepAlive     → 0000" $ KeepAlive      `encodesTo` [0, 0, 0, 0]
      it "Choke         → 00010" $ Choke         `encodesTo` [0, 0, 0, 1, 0]
      it "Unchoke       → 00011" $ Unchoke       `encodesTo` [0, 0, 0, 1, 1]
      it "Interested    → 00013" $ Interested    `encodesTo` [0, 0, 0, 1, 2]
      it "NotInterested → 00013" $ NotInterested `encodesTo` [0, 0, 0, 1, 3]

    describe "constant IDs" $ do
      it "Choke         → 0"   $ Choke                  `hasId` 0
      it "Unchoke       → 1"   $ Unchoke                `hasId` 1
      it "Interested    → 2"   $ Interested             `hasId` 2
      it "NotInterested → 3"   $ NotInterested          `hasId` 3
      prop "Have x        → 4" $ \x     → Have x        `hasId` 4
      prop "Bitfield x    → 5" $ \x     → Bitfield x    `hasId` 5
      prop "Request x y z → 6" $ \x y z → Request x y z `hasId` 6
      prop "Piece x y z   → 7" $ \x y z → Piece x y z   `hasId` 7
      prop "Cancel x y z  → 8" $ \x y z → Cancel x y z  `hasId` 8
      prop "Port x        → 9" $ \x     → Port x        `hasId` 9

    describe "length" $ do
      prop "Four initial bytes describe the remaining length" $ \(Me m) →
        -- Abuse ‘binary’ to convert Word32 → [Word8].
        let m' = unpack $ encodeStrict m
            l32 = fromIntegral $ length (drop 4 m')
            l8 = runGet (replicateM 4 getWord8) $ runPut (putWord32be l32)
        in take 4 m' `shouldBe` l8

newtype Me = Me { _unMe ∷ Message } deriving (Show, Eq)

instance Arbitrary Me where
  arbitrary = choose (0 ∷ Word8, 9) >>= \case
    0 → return $ Me KeepAlive
    1 → return $ Me Choke
    2 → return $ Me Unchoke
    3 → return $ Me Interested
    4 → Me . Have <$> arbitrary
    5 → Me . Bitfield <$> arbitrary
    6 → Me <$> liftA3 Request arbitrary arbitrary arbitrary
    7 → Me <$> liftA3 Piece arbitrary arbitrary arbitrary
    8 → Me <$> liftA3 Cancel arbitrary arbitrary arbitrary
    9 → Me . Port <$> arbitrary
    n → fail $ "Aribtrary Me somehow rolled out of 0-9 range: " ++ show n
