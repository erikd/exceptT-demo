module Improved.Dog
  ( Dog (..)
  , DogParseError (..)
  , parseDog
  , renderDogParseError
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text, pack)

-- A trivial Dog data type
data Dog = Dog

-- An error type.
data DogParseError
  = DogParseErrorOne
  | DogParseErrorTwo
  deriving Show

-- A trival parser for a Dog data type.
-- This can't fail, but obviously a real parser could.
parseDog :: ByteString -> Either DogParseError Dog
parseDog = const $ Right Dog

-- Would not use `show` for a real error type.
renderDogParseError :: DogParseError -> Text
renderDogParseError = pack . show
