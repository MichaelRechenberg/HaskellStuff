
-- Hspec test suite, make sure you modified build-depends in the .cabal file
import Test.Hspec
-- The code that we wrote in src/, make sure you modified .cabal to expose that module
import CoinChange


main :: IO ()
main = hspec $ do
  describe "Dispensing Change" $ do
    it "passes a sanity check" $ 
      2+2 `shouldBe` 4
    it "dispenses change correctly for 1 cent" $
      coinChange 1 coins `shouldBe` 1
    it "dispenses change correctly for 5 cents" $
      coinChange 5 coins `shouldBe` 2
    it "dispenses change correctly for 6 cents" $
      coinChange 6 coins `shouldBe` 2
    it "dispenses change correctly for 10 cents" $
      coinChange 10 coins `shouldBe` 4
    it "dispenses change correctly for 74 cents" $
      coinChange 74 coins `shouldBe` 112
    it "dispenses change correctly for 100 cents" $
      coinChange 100 coins `shouldBe` 292
  describe "Dispensing change (memoized)" $ do
    it "dispenses change correctly for 1 cent" $
      coinChangeMemo 1 coins `shouldBe` 1
    it "dispenses change correctly for 5 cents" $
      coinChangeMemo 5 coins `shouldBe` 2
    it "dispenses change correctly for 6 cents" $
      coinChangeMemo 6 coins `shouldBe` 2
    it "dispenses change correctly for 10 cents" $
      coinChangeMemo 10 coins `shouldBe` 4
    it "dispenses change correctly for 74 cents" $
      coinChangeMemo 74 coins `shouldBe` 112
    it "dispenses change correctly for 100 cents" $
      coinChangeMemo 100 coins `shouldBe` 292
    it "dispenses change correctly for 1000 cents" $
      coinChangeMemo 1000 coins `shouldBe` 801451
  where
    --Had to define coins in a where block
    coins = [1, 5, 10, 25, 50]
