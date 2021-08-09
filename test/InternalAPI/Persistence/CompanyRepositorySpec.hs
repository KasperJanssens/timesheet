module InternalAPI.Persistence.CompanyRepositorySpec where

import           Data.Time                                 (getCurrentTime,
                                                            toGregorian,
                                                            utctDay)
import qualified InternalAPI.Persistence.CompanyRepository as CompanyRepository
import           Test.Sandwich
import Control.Monad.IO.Class (liftIO)

unitSpec :: CoreSpec
unitSpec = describe "up with number should correctly increase number" $ do
  it "should work when nothing present" $ do
    today <- liftIO getCurrentTime
    let (curYear, _, _) = toGregorian $ utctDay today
    let newNumber = CompanyRepository.upWithNumber today Nothing
    newNumber `shouldBe` (fromIntegral curYear * 1000 + 1)
  it "should work when new year" $ do
    today <- liftIO getCurrentTime
    let (curYear, _, _) = toGregorian $ utctDay today
    let newNumber = CompanyRepository.upWithNumber today (Just ((fromIntegral curYear - 1) * 1000 + 13))
    newNumber `shouldBe` (fromIntegral curYear * 1000 + 1)
  it "should calculate next moment correctly" $ do
    today <- liftIO getCurrentTime
    let (curYear, _, _) = toGregorian $ utctDay today
    let newNumber = CompanyRepository.upWithNumber today (Just (fromIntegral curYear  * 1000 + 1))
    newNumber `shouldBe` (fromIntegral curYear * 1000 + 2)    
