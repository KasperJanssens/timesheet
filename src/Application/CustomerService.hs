{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Application.CustomerService where

import           Application.Environment
import           Control.Monad                              (void)
import           Control.Monad.Error.Class                  (throwError)
import           Control.Monad.IO.Class                     (liftIO)
import           Control.Monad.Trans.Reader                 (asks)
import           Data.Time.Calendar                         (Day, addDays)
import           Data.UUID                                  (UUID)
import qualified Data.UUID.V4                               as UUID
import           Database.Persist.Postgresql                (ConnectionString,
                                                             withPostgresqlPool)
import           Domain.Customer
import           ExternalAPI.NewTypes.NewCustomer           (NewCustomer (..))
import           InternalAPI.Persistence.BusinessId
import qualified InternalAPI.Persistence.CustomerRepository as CustomerRepository
import qualified InternalAPI.Persistence.Database           as DB
import           Numeric.Natural                            (Natural)
import           Servant.Server.Internal.ServerError        (err404, errBody)

determinePaymentDate :: Day -> UUID -> AppM Day
determinePaymentDate today customerId = do
  maybeCustomer <- get customerId
  maybe (throwError $ err404 {errBody = "Could not find customer"}) (return . determinePaymentDate' today) maybeCustomer

determinePaymentDate' :: Day -> Customer -> Day
determinePaymentDate' today customer = addDays (toInteger $ Domain.Customer.paymentTerm customer) today

list :: Natural -> Natural -> AppM (Int, [Customer])
list from to = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      dailies <- CustomerRepository.getCustomers (fromEnum from) (fromEnum to)
      total <- CustomerRepository.countCustomers
      return (total, dailies)

get :: UUID -> AppM (Maybe Customer)
get businessId = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      CustomerRepository.findByBusinessId $ BusinessId businessId

delete :: UUID -> AppM (Maybe Customer)
delete businessId = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      maybeDay <- CustomerRepository.findByBusinessId $ BusinessId businessId
      CustomerRepository.deleteCustomer $ BusinessId businessId
      return maybeDay

--TODO no diff anymore between daily and newDay. Keep for future or remove?
insert :: NewCustomer -> AppM Customer
insert (NewCustomer n vat addressStreet addressCity h paymentTerm) = do
  pool <- asks poel
  businessId <- liftIO UUID.nextRandom
  let customer = Customer businessId n addressStreet addressCity vat h paymentTerm
  void $
    DB.executeInPool pool $
      do
        CustomerRepository.insertCustomer customer

  return customer

listAll :: AppM [Customer]
listAll = do
  pool <- asks poel
  DB.executeInPool pool CustomerRepository.getAllCustomers
