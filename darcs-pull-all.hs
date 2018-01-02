#! /usr/bin/env runhaskell

{-
   This is intended to be run regularly from a cron job or
   similar. Something like this is what I use:

      00 04 * * *  sh -c '/path/to/git-pull-all.hs /var/lib/git >> /path/to/git-pull-all.log' || echo "ERROR exit code: $?"
-}

import Control.Monad ( filterM )
import Data.List ( isPrefixOf, sort )
import Data.Time.Clock ( getCurrentTime )
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Time.LocalTime ( utcToLocalZonedTime )
import System.Directory ( getDirectoryContents, setCurrentDirectory )
import System.Environment ( getArgs, getProgName )
import System.Exit ( ExitCode ( ExitSuccess ) )
import System.FilePath
import System.IO ( BufferMode ( NoBuffering )
   , hSetBuffering, stdin, stdout, stderr )
import System.Process ( system )
import Text.Printf ( printf )


main :: IO ()
main = do
   gitDir <- getArgs >>= parseArgs

   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr, stdin ]

   logM "Starting"

   allProjects <-
      ( sort                           -- ..sorted
      . filter (not . isPrefixOf ".")  -- ..excluding dot dirs
      ) `fmap`
      getDirectoryContents gitDir    -- All git repos here..

   -- Turn them into absolute paths
   let allProjectsAbs = map (gitDir </>) allProjects

   -- Only repos that have a remote set
   --remoteProjects <- filterM hasRemote allProjectsAbs

   -- Pull in each of them
   --mapM_ pull remoteProjects

   -- Perform a `git remote update` in each of them
   mapM_ update allProjectsAbs

   logM "Completed"


parseArgs :: [String] -> IO FilePath
parseArgs ("-h"     : _ ) = usage
parseArgs ("--help" : _ ) = usage
parseArgs (p        : []) = return p
parseArgs _               = usage


usage :: IO a
usage = do
   pn <- getProgName
   error $ printf "usage: %s DIR_CONTAINING_DARCS_REPOS" pn


update :: FilePath -> IO Bool
update path = do
   setCurrentDirectory path
   ok <$> system "git remote update"


hasRemote :: FilePath -> IO Bool
hasRemote path = do
   setCurrentDirectory path
   ok `fmap` system "darcs show repo | grep -q Remote"


pull :: FilePath -> IO Bool
pull path = do
   setCurrentDirectory path

   -- Could optionally add --verbose to see the patch descriptions
   ok `fmap` system "darcs pull --all 2>&1"


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
