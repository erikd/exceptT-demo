module Naive.Dog
  ( Dog (..)
  , parseDog
  ) where

import Data.ByteString (ByteString)

-- A trivial Dog data type
data Dog = Dog

-- A trival parser for a Dog data type.
-- This can't fail, but obviously a real parser could.
parseDog :: ByteString -> Dog
parseDog = const Dog
