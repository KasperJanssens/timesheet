{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception                (Exception, throw,
                                                   toException)
import           ExternalAPI.Server
import qualified InternalAPI.Persistence.Database as Database
import           SmallDPProblemn

data MyArithException = DivByZero | OtherArithException
  deriving (Show, Exception)

pureThrow :: Int
pureThrow = throw DivByZero

main :: IO ()
main = do
  runApp

runApp :: IO ()
runApp = do
  let password = "mysecretpassword"
  let user = "postgres"
  let dbLessConn = Database.connDbless "localhost" user password 9875
  Database.provisionalCreateDatabase dbLessConn
  let dbConn = Database.conn "localhost" "timesheetdb" user password 9875
  Database.migrate dbConn

  startServer 9876 9875
