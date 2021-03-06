
import Data.ByteString.Char8 (readFile, writeFile)

import Naive.Cat (Cat, parseCat)
import Naive.Db (Result, processWithDb, renderResult, withDatabaseConnection)
import Naive.Dog (Dog, parseDog)

import Prelude hiding (readFile, writeFile)

import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inFile1, infile2, outFile] -> processFiles inFile1 infile2 outFile
    _ -> putStrLn "Expected three file names." >> exitFailure

readCatFile :: FilePath -> IO Cat
readCatFile fpath = do
  putStrLn $ "Reading Cat file '" ++ fpath ++ "'."
  parseCat <$> readFile fpath

readDogFile :: FilePath -> IO Dog
readDogFile fpath = do
  putStrLn $ "Reading Dog file '" ++ fpath ++ "'."
  parseDog <$> readFile fpath

writeResultFile :: FilePath -> Result -> IO ()
writeResultFile fpath result = do
  putStrLn $ "Writing Result file '" ++ fpath ++ "'."
  writeFile fpath $ renderResult result

processFiles :: FilePath -> FilePath -> FilePath -> IO ()
processFiles infile1 infile2 outfile = do
  cat <- readCatFile infile1
  dog <- readDogFile infile2
  result <- withDatabaseConnection $ \ db ->
               processWithDb db cat dog
  writeResultFile outfile result
