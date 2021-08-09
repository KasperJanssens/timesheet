{-# LANGUAGE OverloadedStrings #-}

module Domain.VATSpec where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson.Types       (fromJSON, toJSON)
import           Data.Either            (isRight)
import           Domain.VAT             (VAT (..))
import qualified Domain.VAT             as VAT
import           Test.Sandwich
import Helper.TestHelper

spec :: CoreSpec
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
    liftIO $ print $ VAT.prettyPrint myVat
    let jsonVat = toJSON myVat
    let parseResult = fromJSON jsonVat
    parseResult `shouldBe` pure myVat

