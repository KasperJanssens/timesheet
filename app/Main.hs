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

--  print $ runCalc 6
--  runBiggerProblem
--  runLongestIncreasing

runApp :: IO ()
runApp = do
  let password = "mysecretpassword"
  let user = "postgres"
  let dbLessConn = Database.connDbless "localhost" user password 5432
  Database.provisionalCreateDatabase dbLessConn
  let dbConn = Database.conn "localhost" "timesheetdb" user password 5432
  Database.migrate dbConn

  start 9876
