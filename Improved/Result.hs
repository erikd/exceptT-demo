{-# LANGUAGE OverloadedStrings  #-}
module Improved.Result
  ( Result (..)
  , ResultError (..)
  , processWithDb
  , renderResult
  , renderResultError
  ) where

import Data.ByteString.Char8 (ByteString, pack)

import Improved.Cat
import Improved.Db
import Improved.Dog

-- A trivial Result data type
data Result = Result

data ResultError
  = ResultError1
  | ResultError2
  deriving Show

-- Imagine this function queries a database about the Cat and Dog data and
-- then constructs anre returns a Result.
-- In this formulation errors would be returned as exceptions or calls to
-- `error`.
processWithDb :: DbConn -> Cat -> Dog -> IO Result
processWithDb _ _ _ = pure Result

-- If Result was a little more complex we could have a proper render function.
renderResult :: Result -> ByteString
renderResult = const "result"

renderResultError :: ResultError -> ByteString
renderResultError = pack . show
