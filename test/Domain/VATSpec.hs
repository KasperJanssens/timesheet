{-# LANGUAGE OverloadedStrings #-}

module Domain.VATSpec where

import Data.Either (isRight)
import qualified Domain.VAT as VAT
import Domain.VAT(VAT(..))
import Test.Hspec
import Data.Aeson.Types (toJSON, fromJSON)

spec :: Spec
spec = describe "calculate vat numbers" $ do
  it "should recognize a correct dutch number" $ do
    let res = VAT.parse "NL810433941B01"
    res `shouldSatisfy` isRight
    res `shouldBe` Right (NL 81043394 1 01)
    let res2 = VAT.parse "BE0893815606"
    res2 `shouldSatisfy` isRight
    res2 `shouldBe` Right (BE 8938156 06)
  it "should nicely go to and from json" $ do
    let myVat = BE 8938156 06
    print $ VAT.prettyPrint myVat
    let jsonVat = toJSON myVat
    let parseResult = fromJSON jsonVat
    parseResult `shouldBe` pure myVat
    
