
import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Error

import Data.ByteString.Char8 (readFile, writeFile)

import Improved.Cat (Cat, CatParseError, parseCat)
import Improved.Db (withDatabaseConnection)
import Improved.Dog (Dog, DogParseError, parseDog)
import Improved.Result (Result, processWithDb, renderResult)

import Prelude hiding (readFile, writeFile)

import System.Environment (getArgs)
import System.Exit (exitFailure)

data ProcessError
  = ECat CatParseError
  | EDog DogParseError
  | EReadFile FilePath String
  | EWriteFile FilePath String


main :: IO ()
main = do
  args <- getArgs
  case args of
    [inFile1, infile2, outFile] ->
            report =<< runExceptT (processFiles inFile1 infile2 outFile)
    _ -> putStrLn "Expected three file names." >> exitFailure

report :: Either ProcessError () -> IO ()
report (Right _) = pure ()
report (Left e) =
  putStrLn $
    case e of
      ECat _ -> "Cat parse error."
      EDog _ -> "Dog parse error."
      EReadFile fpath msg -> "Error reading '" ++ fpath ++ "' : " ++ msg
      EWriteFile fpath msg -> "Error writing '" ++ fpath ++ "' : " ++ msg


readCatFile :: FilePath -> ExceptT ProcessError IO Cat
readCatFile fpath = do
  liftIO $ putStrLn "Reading Cat file."
  bs <- handleExceptT handler $ readFile fpath
  hoistEither . fmapL ECat $ parseCat bs
  where
    handler :: SomeException -> ProcessError
    handler e = EReadFile fpath (show e)

readDogFile :: FilePath -> ExceptT ProcessError IO Dog
readDogFile fpath = do
  liftIO $ putStrLn "Reading Dog file."
  bs <- handleExceptT handler $ readFile fpath
  hoistEither . fmapL EDog $ parseDog bs
  where
    handler :: SomeException -> ProcessError
    handler e = EReadFile fpath (show e)

writeResultFile :: FilePath -> Result -> ExceptT ProcessError IO ()
writeResultFile fpath result = do
  liftIO $ putStrLn "Writing Result file."
  handleExceptT handler . writeFile fpath $ renderResult result
  where
    handler :: SomeException -> ProcessError
    handler e = EWriteFile fpath (show e)

processFiles :: FilePath -> FilePath -> FilePath -> ExceptT ProcessError IO ()
processFiles infile1 infile2 outfile = do
  cat <- readCatFile infile1
  dog <- readDogFile infile2
  result <- liftIO . withDatabaseConnection $ \ db ->
               processWithDb db cat dog
  writeResultFile outfile result
