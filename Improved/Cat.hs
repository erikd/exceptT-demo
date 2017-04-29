module Improved.Cat
  ( Cat (..)
  , CatParseError (..)
  , parseCat
  , renderCatParseError
  ) where

import Data.ByteString.Char8 (ByteString, pack)

-- A trivial Cat data type
data Cat = Cat

-- An error type.
data CatParseError
  = CatParseErrorOne
  | CatParseErrorTwo
  deriving Show

-- A trival parser for a Cat data type.
-- This can't fail, but obviously a real parser could.
parseCat :: ByteString -> Either CatParseError Cat
parseCat = const $ Right Cat

-- Would not use `show` for a real error type.
renderCatParseError :: CatParseError -> ByteString
renderCatParseError = pack . show
