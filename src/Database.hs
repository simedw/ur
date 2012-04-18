module Database
  ( Database
  , FunctionName
  , LibraryName
  , getLibraries
  , append
  , load
  , save
  , empty
  , lookup
  ) where

import Prelude hiding (lookup)

-- to run shell scripts
import HSH hiding (space)
import System.FilePath
import System.Exit (ExitCode)

import Control.Applicative ((<$>),(<*), (*>), pure)
import Control.Monad
import Data.Maybe

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

-- to store the data
import Data.Map (Map)
import qualified Data.Map as M
import Data.Serialize (encode, decode)

-- for parsing nm output
import Text.Parsec.ByteString
import Text.Parsec


type LazyHSH = IO (String, IO (String, ExitCode))
type P       = Parser

type Database     = Map FunctionName [LibraryName]
type LibraryName  = ByteString
type FunctionName = ByteString

-- will return both .so and .a libraries
-- but if libx.a and libx.so exists only libx.a will be returned
getLibraries :: FilePath -> IO [FilePath]
getLibraries path = do
    static  <- lines . fst <$> (run ("find " ++ path </> "lib*.a" ) :: LazyHSH)
    dynamic <- lines . fst <$> (run ("find " ++ path </> "lib*.so") :: LazyHSH)

    let tS       = map (flip replaceExtension ".so") static
    let dynamic' = filter (not . flip elem tS) dynamic

    return (static ++ dynamic')

getRawNM :: FilePath -> IO ByteString
getRawNM path | takeExtension path == ".a" 
    = run ("nm " ++ path ++ " 2> /dev/null")
getRawNM path | takeExtension path == ".so" 
    = run ("nm --dynamic " ++ path ++ " 2> /dev/null")

parseRawNM :: ByteString -> [FunctionName]
parseRawNM input = case parse parser "" input of
    Left  err -> error $ "parseRawNM: " ++ show err
    Right suc -> catMaybes suc
  where
    parser = many (try pFunctionDecl <|> pSkip)

pFunctionAddr :: P ()
pFunctionAddr = replicateM 16 hexDigit *> return ()

pFunctionDecl :: P (Maybe FunctionName)
pFunctionDecl = do
    pFunctionAddr
    space
    l <- letter
    unless (include l) $ fail $ show l ++ " is not a defintion"
    space
    Just . BC.pack <$> many1 (letter <|> char '_' <|> char '_' <|> digit) 

pSkip :: P (Maybe a)
pSkip = manyTill anyChar newline *> pure Nothing

-- only include definitions of certain kind
include :: Char -> Bool
include = flip elem "tTW"

getLibraryName :: FilePath -> ByteString
getLibraryName path = BC.pack $ drop 3 filename -- remove 'lib'
  where filename = takeBaseName path

addLibrary :: LibraryName -> [FunctionName] -> Database -> Database
addLibrary name []     db = db
addLibrary name (f:fs) db = insertFunction f name $ addLibrary name fs db
  where insertFunction k v = M.insertWith (++) k [v]

append :: [FilePath] -> Database -> IO Database
append libraries db = M.unions <$> mapM help libraries
  where help lib = flip (addLibrary (getLibraryName lib)) empty
                 . parseRawNM <$> getRawNM lib

empty :: Database
empty = M.empty

save :: FilePath -> Database -> IO ()
save path db = B.writeFile path (encode db)

load :: FilePath -> IO Database
load path = do
    res <- B.readFile path
    case decode res of
        Left str -> error  $ "load: " ++ str
        Right db -> return db


lookup :: FunctionName -> Database -> Maybe [LibraryName]
lookup = M.lookup

