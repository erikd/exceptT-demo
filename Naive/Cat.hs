module Naive.Cat
  ( Cat (..)
  , parseCat
  ) where

-- A trivial Cat data type
data Cat = Cat

-- A trival parser for a Cat data type.
-- This can't fail, but obviously a real parser could.
parseCat :: String -> Cat
parseCat = const Cat
