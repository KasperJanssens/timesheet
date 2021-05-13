module Application.QuoteUseCaseSpec where

import Test.Hspec

spec :: Spec
spec = describe "quote use case spec" $ do
  it "list active quotes, invoice one, have one less active quote" $ do
    True `shouldBe` False