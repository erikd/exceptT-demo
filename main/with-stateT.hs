import Control.Exception
import Control.Monad.State.Strict
import Data.Typeable

-- Control.Error is from the errors == 2.2.* package.
import Control.Error (ExceptT, handleExceptT, runExceptT)

-- Inspired by:
--    https://www.fpcomplete.com/blog/2017/06/tale-of-two-brackets
--    https://www.reddit.com/r/haskell/comments/6jk3vy/a_tale_of_two_brackets/


data OddException = OddException !Int -- great name :)
  deriving (Show, Typeable)
instance Exception OddException

data OddError = OddError !Int
  deriving Show

mayThrow :: StateT Int (ExceptT OddError IO) Int
mayThrow = do
  x <- get
  if odd x
    then lift $ handleExceptT oddHandler $ throwIO $ OddException x
    else do
      put $! x + 1
      return $ x `div` 2

oddHandler :: OddException -> OddError
oddHandler (OddException i) = OddError i


main :: IO ()
main =
  print =<< runExceptT (runStateT (replicateM 2 mayThrow) 0)
