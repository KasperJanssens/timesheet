{-# LANGUAGE TypeFamilies #-}

module Application.CustomerService where


import           Application.Environment
import           Control.Monad                          (void)
import           Control.Monad.Logger                   (runStderrLoggingT)
import           Control.Monad.Trans.Reader             (asks)
import           Data.UUID                              (UUID)
import qualified Data.UUID.V4                           as UUID
import           Database.Persist.Postgresql            (ConnectionString,
                                                         withPostgresqlPool)
import           Domain.Customer
import           ExternalAPI.NewTypes.NewCustomer       (NewCustomer (..))
import           InternalAPI.Persistence.BusinessId
import qualified InternalAPI.Persistence.CustomerRepository as CustomerDatabase
import qualified InternalAPI.Persistence.Database       as DB
import           Numeric.Natural                        (Natural)
import Control.Monad.IO.Class (liftIO)

list :: Natural -> Natural -> AppM (Int, [Customer])
list from to = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      dailies <- CustomerDatabase.getCustomers (fromEnum from) (fromEnum to)
      total <- CustomerDatabase.countCustomers
      return (total, dailies)

get :: UUID -> AppM (Maybe Customer)
get businessId = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      CustomerDatabase.findByBusinessId  $ BusinessId businessId

delete :: UUID -> AppM (Maybe Customer)
delete businessId = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      maybeDay <- CustomerDatabase.findByBusinessId   $ BusinessId businessId
      CustomerDatabase.deleteCustomer   $ BusinessId businessId
      return maybeDay

--TODO no diff anymore between daily and newDay. Keep for future or remove?
insert :: NewCustomer -> AppM Customer
insert (NewCustomer n vat h paymentTerm) = do
  pool <- asks poel
  businessId <- liftIO UUID.nextRandom
  let customer = Customer businessId n vat h paymentTerm
  void $ DB.executeInPool pool $
    do
      CustomerDatabase.insertCustomer customer

  return customer
