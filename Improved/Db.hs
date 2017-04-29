{-# LANGUAGE OverloadedStrings #-}
module Improved.Db
  ( DbConn (..)
  , DbError (..)
  , Result (..)
  , ResultError (..)
  , processWithDb
  , renderDbError
  , renderResult
  , renderResultError
  , withDatabaseConnection
  ) where


import Control.Error (ExceptT, fmapLT)
import Control.Monad.IO.Class (liftIO)

import Data.ByteString (ByteString)
import Data.Text (Text, pack)

import Improved.Cat
import Improved.Dog


-- A database connection.
data DbConn = DbConn

-- A trivial Result data type
data Result = Result

data DbError
  = DbError1
  | DbError2
  | DbError3 ResultError
  deriving Show

data ResultError
  = ResultError1
  | ResultError2
  deriving Show


withDatabaseConnection :: (DbConn -> ExceptT ResultError IO a) -> ExceptT DbError IO a
withDatabaseConnection action =
  fmapLT DbError3 $ action DbConn


-- Imagine this function queries a database about the Cat and Dog data and
-- then constructs anre returns a Result.
-- In this formulation errors would be returned as exceptions or calls to
-- `error`.
processWithDb :: DbConn -> Cat -> Dog -> ExceptT ResultError IO Result
processWithDb _ _ _ = liftIO $ pure Result

-- If Result was a little more complex we could have a proper render function.
renderResult :: Result -> ByteString
renderResult = const "result"



-- Would not use show for anything other than an example.
renderDbError :: DbError -> Text
renderDbError dbe =
  case dbe of
    DbError3 re -> renderResultError re
    _ -> pack $ show dbe

renderResultError :: ResultError -> Text
renderResultError = pack . show
