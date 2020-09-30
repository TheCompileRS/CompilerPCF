import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)


main :: IO ()
main = hspec $ do
  describe "Parse" $ do
    it "checks that sum is commutative" $ do
      property $ \x y -> x + y == (y + x :: Int)
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

