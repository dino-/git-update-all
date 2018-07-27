#! /usr/bin/env runhaskell

{-
   This is intended to be run regularly from a cron job or
   similar. Something like this is what I use:

      00 04 * * *  sh -c '/path/to/git-update-all.hs /var/lib/git >> /path/to/git-update-all.log' || echo "ERROR exit code: $?"

   The directories this script updates should be mirror clones of the originals to ensure it gets all changes. To make this type of clone:

      $ git clone --mirror https://path/to/project.git
-}

import Control.Monad ( forM )
import Data.Char ( toLower )
import Data.List ( isPrefixOf, sortOn )
import Data.Time.Clock ( getCurrentTime )
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Time.LocalTime ( utcToLocalZonedTime )
import System.Directory ( getDirectoryContents, setCurrentDirectory )
import System.Environment ( getArgs, getProgName )
import System.Exit ( ExitCode ( ExitSuccess ), die, exitSuccess )
import System.FilePath ( (</>) )
import System.IO ( BufferMode ( NoBuffering ),
   hSetBuffering, stdout, stderr )
import System.Process ( system )
import Text.Printf ( printf )


main :: IO ()
main = do
   gitDir <- getArgs >>= parseArgs

   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   logM "Starting"

   allProjects <-
      ( sortOn (map toLower)           -- ..sorted
      . filter (not . isPrefixOf ".")  -- ..excluding dot dirs
      ) `fmap`
      getDirectoryContents gitDir      -- All git repos here..

   -- Turn them into absolute paths
   let allProjectsAbs = map (gitDir </>) allProjects

   -- Perform a `git remote update` in each of them
   results <- forM allProjectsAbs $ \path -> do
      printf "Updating repository at %s\n" path
      setCurrentDirectory path
      system "git remote update"

   logM "Completed"

   if (any (/= ExitSuccess) results)
      then die "Failed to update one or more git repositories. Please check the log."
      else exitSuccess


parseArgs :: [String] -> IO FilePath
parseArgs ("-h"     : _ ) = usage
parseArgs ("--help" : _ ) = usage
parseArgs (p        : []) = return p
parseArgs _               = usage


usage :: IO a
usage = do
   pn <- getProgName
   die $ printf "usage: %s DIR_CONTAINING_GIT_REPOS" pn


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
