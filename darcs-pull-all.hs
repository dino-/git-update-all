#! /usr/bin/env runhaskell

import Control.Monad
import Data.List
import Data.Time
import System.Cmd
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO ( BufferMode ( NoBuffering )
                 , hSetBuffering, stdin, stdout, stderr 
                 )
import System.Locale
import Text.Printf


main :: IO ()
main = do
   darcsDir : [] <- getArgs

   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr, stdin ]

   logM "Starting"

   allProjects <-
      ( sort                           -- ..sorted
      . filter (not . isPrefixOf ".")  -- ..excluding dot dirs
      ) `fmap`
      getDirectoryContents darcsDir    -- All darcs repos here..

   -- Turn them into absolute paths
   let allProjectsAbs = map (darcsDir </>) allProjects

   -- Only repos that have a remote set
   remoteProjects <- filterM hasRemote allProjectsAbs

   -- Pull in each of them
   mapM_ pull remoteProjects

   logM "Completed"


hasRemote :: FilePath -> IO Bool
hasRemote path = do
   setCurrentDirectory path
   ok `fmap` system "darcs show repo | grep -q Remote"


pull :: FilePath -> IO Bool
pull path = do
   setCurrentDirectory path

   -- Could optionally add --verbose to see the patch descriptions
   ok `fmap` system "darcs pull --all"


{- Get the current date/time as a string in the specified format
   For format string help, see man 3 strftime
-}
dateFormat :: String -> IO String
dateFormat fmt = formatTime defaultTimeLocale fmt `fmap`
   (getCurrentTime >>= utcToLocalZonedTime)


{- Output a message with datestamp
-}
logM :: String -> IO ()
logM msg = do
   tstamp <- dateFormat "%F %T"
   printf "%s> %s\n" tstamp msg


{- Turn an exit code (say, from system) into a Bool
-}
ok :: ExitCode -> Bool
ok ExitSuccess = True
ok _           = False
