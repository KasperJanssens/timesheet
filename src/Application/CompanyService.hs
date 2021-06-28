module Application.CompanyService where

import           Application.Environment                   (AppM, poel)
import           Control.Monad.IO.Class
import           Control.Monad.RWS.Class                   (asks)
import           Data.UUID                                 (UUID)
import qualified Data.UUID.V4                              as UUID
import           Domain.Company
import           ExternalAPI.NewTypes.NewCompany
import           GHC.Natural                               (Natural)
import           InternalAPI.Persistence.BusinessId
import qualified InternalAPI.Persistence.CompanyRepository as CompanyRepository
import qualified InternalAPI.Persistence.Database          as DB



list :: Natural -> Natural -> AppM (Int, [Company])
list from to = do
  pool <- asks poel
  DB.executeInPool pool $ do
    companies <- CompanyRepository.getCompanies (fromEnum from) (fromEnum to)
    amount <- CompanyRepository.countCompanies
    return (amount, companies)

insert :: NewCompany -> AppM Company
insert newCompany = do
  pool <- asks poel
  uuid <- liftIO UUID.nextRandom
  DB.executeInPool pool $ do
    CompanyRepository.insertCompany (BusinessId uuid) newCompany

get :: BusinessId Company -> AppM (Maybe Company)
get externalBusinessId = do
  pool <- asks poel
  DB.executeInPool pool $ do
    CompanyRepository.getCompany externalBusinessId

listAll :: AppM [Company]
listAll = do
  pool <- asks poel
  DB.executeInPool pool CompanyRepository.getAllCompanies
