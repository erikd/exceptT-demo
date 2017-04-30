{-# LANGUAGE OverloadedStrings #-}
import           Control.Exception (SomeException)
import           Control.Monad.IO.Class (liftIO)
import           Control.Error (ExceptT, fmapL, fmapLT, handleExceptT, hoistEither, runExceptT)

import           Data.ByteString.Char8 (readFile, writeFile)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Improved.Cat (Cat, CatParseError, parseCat, renderCatParseError)
import           Improved.Db (DbError, Result, processWithDb, renderDbError, renderResult, withDatabaseConnection)
import           Improved.Dog (Dog, DogParseError, parseDog, renderDogParseError)

import           Prelude hiding (readFile, writeFile)

import           System.Environment (getArgs)
import           System.Exit (exitFailure)

data ProcessError
  = ECat CatParseError
  | EDog DogParseError
  | EReadFile FilePath Text
  | EWriteFile FilePath Text
  | EDb DbError

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inFile1, infile2, outFile] ->
            report =<< runExceptT (processFiles inFile1 infile2 outFile)
    _ -> do
        putStrLn "Expected three file names, the first two are input, the last output."
        exitFailure

report :: Either ProcessError () -> IO ()
report (Right _) = pure ()
report (Left e) = T.putStrLn $ renderProcessError e


renderProcessError :: ProcessError -> Text
renderProcessError pe =
  case pe of
    ECat ec -> renderCatParseError ec
    EDog ed -> renderDogParseError ed
    EReadFile fpath msg -> "Error reading '" <> T.pack fpath <> "' : " <> msg
    EWriteFile fpath msg -> "Error writing '" <> T.pack fpath <> "' : " <> msg
    EDb dbe -> renderDbError dbe


readCatFile :: FilePath -> ExceptT ProcessError IO Cat
readCatFile fpath = do
  liftIO . putStrLn $ "Reading Cat file '" ++ fpath ++ "'."
  bs <- handleExceptT handler $ readFile fpath
  hoistEither . fmapL ECat $ parseCat bs
  where
    handler :: SomeException -> ProcessError
    handler e = EReadFile fpath (T.pack $ show e)

readDogFile :: FilePath -> ExceptT ProcessError IO Dog
readDogFile fpath = do
  liftIO . putStrLn $ "Reading Dog file '" ++ fpath ++ "'."
  bs <- handleExceptT handler $ readFile fpath
  hoistEither . fmapL EDog $ parseDog bs
  where
    handler :: SomeException -> ProcessError
    handler e = EReadFile fpath (T.pack $ show e)

writeResultFile :: FilePath -> Result -> ExceptT ProcessError IO ()
writeResultFile fpath result = do
  liftIO . putStrLn $ "Writing Result file '" ++ fpath ++ "'."
  handleExceptT handler . writeFile fpath $ renderResult result
  where
    handler :: SomeException -> ProcessError
    handler e = EWriteFile fpath (T.pack $ show e)

processFiles :: FilePath -> FilePath -> FilePath -> ExceptT ProcessError IO ()
processFiles infile1 infile2 outfile = do
  cat <- readCatFile infile1
  dog <- readDogFile infile2
  result <- fmapLT EDb . withDatabaseConnection $ \ db ->
               processWithDb db cat dog
  writeResultFile outfile result
