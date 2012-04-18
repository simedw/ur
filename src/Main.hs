{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Database as D
import Database (Database, FunctionName, LibraryName)

import System.IO
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit
import System.Directory
import System.FilePath ((</>))

import Text.Regex.PCRE.ByteString
import Text.Regex.PCRE

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Data.List
import Data.Either

import Control.Monad
import Control.Applicative ((<$>),(<*), (*>), pure)

data Options = Options  
    { optRebuildDatabase :: Bool
    , optAppendDatabase  :: Bool
    , optDatabaseDir     :: String
    , optLibrarySrc      :: String
    }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "d" ["database"]
        (ReqArg
            (\arg opt -> return opt { optDatabaseDir = arg })
            "FILE")
        "Database path"
    , Option "r" ["rebuild"]
        (NoArg
            (\opt -> return opt { optRebuildDatabase = True }))
        "Rebuild database"
    , Option "a" ["append"]
         (NoArg
            (\opt -> return opt { optAppendDatabase = True }))
        "Append to database"
    , Option "s" ["libraries"]
        (ReqArg
            (\arg opt -> return opt { optLibrarySrc = arg})
            "PATH")
        "Path to libraries"
        
    , Option "v" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.0.1"
                exitWith ExitSuccess))
        "Print version"
 
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]

defaultOptions :: IO Options
defaultOptions = do 
  home <- getHomeDirectory
  return Options 
      { optRebuildDatabase = False
      , optAppendDatabase  = False
      , optDatabaseDir  = home </> ".ur_db"
      , optLibrarySrc   = "/usr/lib/"
      }

modifyDatabase :: Options -> IO ()
modifyDatabase Options{..} | optRebuildDatabase = do
    libs <- D.getLibraries optLibrarySrc 
    putStrLn $ show (length libs) ++ " libraries"
    D.save optDatabaseDir =<< D.append libs D.empty
modifyDatabase Options{..} | optAppendDatabase = do
    libs <- D.getLibraries optLibrarySrc 
    D.load optDatabaseDir >>= D.append libs
                          >>= D.save optDatabaseDir


listen :: Options -> IO ()
listen (Options{..}) = do
    input   <- BC.lines <$> B.getContents
    let missing = nub . sort . concat $ map findMissingRef input

    when (null missing) exitSuccess
    db    <- D.load optDatabaseDir
     
    let (unresolved, found) = partitionEithers $ map (find db) missing
    mapM_ (\library -> putStrLn ("-l" ++ BC.unpack library)) $ nub . sort . concat $ found
    unless (null unresolved) $ do
        putStr $ "The following defintion" ++ s unresolved ++ " eluded me: " 
        putStrLn $ intercalate "," (map BC.unpack unresolved)
 
  where
    s :: [a] -> String
    s xs | length xs > 1 = "s"
    s xs | otherwise     = ""
    find db fname = maybe (Left fname) Right $ D.lookup fname db
    findMissingRef :: ByteString -> [FunctionName]
    findMissingRef str = concat $ map (f . (str =~)) pattern
      where 
      f :: (ByteString, ByteString, ByteString, [ByteString]) -> [ByteString]
      f (_,_,_,x) = x
pattern =
    [ "undefined reference to `([_\\-a-zA-Z0-9]*)'"  -- gcc (ld)
    , "undefined reference to '([_\\-a-zA-Z0-9]*)'"  -- clang
    ]

main = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    opts <- foldl (>>=) (defaultOptions) actions
    
    when (optRebuildDatabase opts && optAppendDatabase opts) $ do
        hPutStrLn stderr "can't rebuild and append at the same time"
        exitFailure
    
    if (optRebuildDatabase opts || optAppendDatabase opts)
      then do 
        putStrLn "This might take a while" 
        modifyDatabase opts
        putStrLn "Done"
        exitSuccess
      else return ()
    
    listen opts
