{-# LANGUAGE OverloadedStrings  #-}
module Improved.Result
  ( Result (..)
  , ResultError (..)
  , processWithDb
  , renderResult
  , renderResultError
  ) where

import Data.ByteString.Char8 (ByteString, pack)

