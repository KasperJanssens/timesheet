module InternalAPI.Persistence.CompanyRepositorySpec where

import           Data.Time                                 (getCurrentTime,
                                                            toGregorian,
                                                            utctDay)
import qualified InternalAPI.Persistence.CompanyRepository as CompanyRepository
import           Test.Hspec

unitSpec :: Spec
unitSpec = describe "up with number should correctly increase number" $ do
  it "should work when nothing present" $ do
    today <- getCurrentTime
    let (curYear, _, _) = toGregorian $ utctDay today
    let newNumber = CompanyRepository.upWithNumber today Nothing
    newNumber `shouldBe` (fromIntegral curYear * 1000 + 1)
  it "should work when new year" $ do
    today <- getCurrentTime
    let (curYear, _, _) = toGregorian $ utctDay today
    let newNumber = CompanyRepository.upWithNumber today (Just ((fromIntegral curYear - 1) * 1000 + 13))
    newNumber `shouldBe` (fromIntegral curYear * 1000 + 1)
  it "should calculate next moment correctly" $ do
    today <- getCurrentTime
    let (curYear, _, _) = toGregorian $ utctDay today
    let newNumber = CompanyRepository.upWithNumber today (Just (fromIntegral curYear  * 1000 + 1))
    newNumber `shouldBe` (fromIntegral curYear * 1000 + 2)    
