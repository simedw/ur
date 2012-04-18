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


import HSH hiding (space)

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


findRef :: Options -> [ByteString] -> IO (Maybe ([FunctionName],[LibraryName]))
findRef (Options{..}) input = do
    db <- D.load optDatabaseDir
    if null missing
        then return Nothing
        else let (fns, libs) = partitionEithers . map (find db) $ missing 
            in return $ Just (fns, nub . sort . concat $ libs)
  where
    missing = nub . sort . concat $ map findMissingRef input
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

listen :: Options -> IO ()
listen opt = do
    input   <- BC.lines <$> B.getContents
    refs <- findRef opt input
    case refs of
        Nothing                     -> exitSuccess
        Just (unresolved, resolved) -> do
            mapM_ (\library -> putStrLn ("-l" ++ BC.unpack library)) $ resolved
            unless (null unresolved) $ do
                putStr $ "The following defintion" ++ s unresolved ++ " eluded me: " 
                putStrLn $ intercalate "," (map BC.unpack unresolved)


s :: [a] -> String
s xs | length xs > 1 = "s"
s xs | otherwise     = ""

toolMode :: Options -> [String] -> IO ()
toolMode opt@(Options{..}) tool = do
  (ss, cc) <- run cmd :: IO (String, IO (String, ExitCode))
  (_,  ec) <- cc
  case ec of
    ExitSuccess   -> return ()
    ExitFailure i -> do
        refs <- findRef opt (BC.lines . BC.pack $ ss)
        case refs of
            Nothing                     -> exitFailure
            Just (unresolved, resolved) | null unresolved -> do
                putStr   $ "rerunning with missing flag" ++ s resolved ++ " : " 
                putStrLn $ intercalate "," (flags resolved)
                run (cmdPlus resolved) :: IO ()
                                    | otherwise       -> do
                putStr   $ "missing flag" ++ s resolved ++ ": " 
                putStrLn $ intercalate "," (flags resolved)
                putStr   $ "unresolved flag" ++ s unresolved ++ ": "
                putStrLn $ intercalate "," (map (BC.unpack) unresolved)
  where 
    cmd        = cmdPlus []
    cmdPlus xs = intercalate " " tool ++ " "
              ++ intercalate " " (flags xs)
              ++ " 2>&1 " -- redirect stdout
    flags      = map ((++) "-l" . BC.unpack)

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
    if null nonOptions
        then listen opts
        else toolMode opts nonOptions
