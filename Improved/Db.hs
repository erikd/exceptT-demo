module Improved.Db
  ( DbConn (..)
  , withDatabaseConnection
  ) where

-- A database connection.
data DbConn = DbConn

withDatabaseConnection :: (DbConn -> IO a) -> IO a
withDatabaseConnection action =
  action DbConn
