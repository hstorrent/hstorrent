module Network.HsTorrent.MainSpec (main, spec) where

import Network.HsTorrent
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "skeleton" $ do
    prop "() == ()" $ \x ->
      f `shouldBe` x
