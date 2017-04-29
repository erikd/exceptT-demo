module Naive.Result
  ( Result (..)
  , processWithDb
  , renderResult
  ) where

import Naive.Cat
import Naive.Db
import Naive.Dog

-- A trivial Result data type
data Result = Result

-- Imagine this function queries a database about the Cat and Dog data and
-- then constructs anre returns a Result.
-- In this formulation errors would be returned as exceptions or calls to
-- `error`.
processWithDb :: DbConn -> Cat -> Dog -> IO Result
processWithDb _ _ _ = pure Result

-- If Result was a little more complex we could have a proper render function.
renderResult :: Result -> String
renderResult = const "result"
