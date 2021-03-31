module Application.CompanyService where

import           Application.Environment                   (AppM, poel)
import           Control.Monad.RWS.Class                   (asks)
import           Domain.Company
import           GHC.Natural                               (Natural)
import qualified InternalAPI.Persistence.CompanyRepository as CompanyRepository
import qualified InternalAPI.Persistence.Database          as DB

list :: Natural -> Natural -> AppM (Int, [Company])
list from to = do
  pool <- asks poel
  DB.executeInPool pool $ do
    companies <- CompanyRepository.getCompanies (fromEnum from) (fromEnum to)
    amount <- CompanyRepository.countCompanies
    return (amount, companies)

insert :: Company -> AppM Company
insert company = do
  pool <- asks poel
  DB.executeInPool pool $ do
    CompanyRepository.insertCompany company
