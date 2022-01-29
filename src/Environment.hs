{-# LANGUAGE NoImplicitPrelude #-}

module Environment
    ( checkConflictStyle, shouldUseColorByTerminal, openEditor
    ) where

import qualified Control.Exception as E
import           Control.Monad (when, unless)
import           Data.Functor ((<&>))
import           Opts (Options(..))
import           PPDiff (ColorEnable(..))
import           StrUtils (stripNewline)
import           System.Console.ANSI (hSupportsANSI)
import           System.IO (stdout)
import           System.Environment (getEnv)
import           System.Exit (ExitCode(..))
import           System.Process (callProcess, readProcessWithExitCode)

import           Prelude.Compat

shouldUseColorByTerminal :: IO ColorEnable
shouldUseColorByTerminal =
    hSupportsANSI stdout
    <&> \istty -> if istty then EnableColor else DisableColor

getConflictStyle :: IO String
getConflictStyle =
    do  (exitCode, output, _) <- readProcessWithExitCode "git" ["config", "merge.conflictstyle"] stdin
        case exitCode of
            ExitSuccess -> pure $ stripNewline output
            ExitFailure 1 -> pure "unset"
            ExitFailure _ -> E.throwIO exitCode
    where
        stdin = ""

setConflictStyle :: IO ()
setConflictStyle =
    callProcess "git" ["config", "--global", "merge.conflictstyle", "diff3"]

checkConflictStyle :: Options -> IO ()
checkConflictStyle opts =
    do  conflictStyle <- getConflictStyle
        unless (conflictStyle `elem` ["diff3", "zdiff3"]) $
            do  unless (shouldSetConflictStyle opts) $
                    fail $ concat
                    [ "merge.conflictstyle must be diff3 but is "
                    , show conflictStyle
                    , ". Use -s to automatically set it globally"
                    ]
                setConflictStyle

                newConflictStyle <- getConflictStyle
                when (newConflictStyle /= "diff3") $
                    fail $ concat
                    [ "Attempt to set conflict style failed. Perhaps you have"
                    , " an incorrect merge.conflictstyle configuration "
                    , "specified in your per-project .git/config?"
                    ]

openEditor :: Options -> FilePath -> IO ()
openEditor opts path
    | shouldUseEditor opts =
        do  editor <- getEnv "EDITOR"
            callProcess editor [path]
    | otherwise = pure ()
