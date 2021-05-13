module Application.QuoteService where

import           Application.Environment
import           Control.Monad.Cont                      (liftIO)
import           Control.Monad.RWS.Class                 (asks)
import           Data.Time.Clock                         (utctDay)
import           Data.Time.Clock.POSIX                   (getCurrentTime)
import           Data.UUID                               (UUID)
import           Domain.Quote
import           ExternalAPI.NewTypes.NewQuote
import qualified InternalAPI.Persistence.Database        as DB
import qualified InternalAPI.Persistence.QuoteRepository as QuoteRepository
import           Numeric.Natural                         (Natural)

listNonInvoiced :: AppM [Quote]
listNonInvoiced = do
  pool <- asks poel
  DB.executeInPool pool $ do
    QuoteRepository.listNonInvoiced
    
    
list :: Natural -> Natural -> AppM (Int, [Quote])
list from to = do
  pool <- asks poel
  DB.executeInPool pool $ do
    invoices <- QuoteRepository.getQuotes (fromEnum from) (fromEnum to)
    amount <- QuoteRepository.countQuotes
    return (amount, invoices)

get :: UUID -> AppM (Maybe Quote)
get id = do
  pool <- asks poel
  DB.executeInPool pool $ QuoteRepository.getQuote id

insert :: NewQuote -> AppM Quote
insert (NewQuote total customerId companyId) = do
  time <- liftIO getCurrentTime
  let today = utctDay time
  pool <- asks poel
  DB.executeInPool pool $ do
    QuoteRepository.insertQuote customerId companyId total
