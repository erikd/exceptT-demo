{-# LANGUAGE OverloadedStrings #-}
module Naive.Db
  ( DbConn (..)
  , Result (..)
  , processWithDb
  , renderResult
  , withDatabaseConnection
  ) where

import Data.ByteString.Char8 (ByteString)

import Naive.Cat
import Naive.Dog

-- A database connection.
data DbConn = DbConn

-- A trivial Result data type
data Result = Result

-- Imagine this function queries a database about the Cat and Dog data and
-- then constructs anre returns a Result.
-- In this formulation errors would be returned as exceptions or calls to
-- `error`.
processWithDb :: DbConn -> Cat -> Dog -> IO Result
processWithDb _ _ _ = pure Result

-- If Result was a little more complex we could have a proper render function.
renderResult :: Result -> ByteString
renderResult = const $ "result"

withDatabaseConnection :: (DbConn -> IO a) -> IO a
withDatabaseConnection action =
  action DbConn

