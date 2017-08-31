{-# LANGUAGE NoImplicitPrelude #-}

module Environment
    ( checkConflictStyle, shouldUseColorByTerminal, openEditor
    ) where

import qualified Control.Exception as E
import           Control.Monad (when, unless)
import           Opts (Options(..))
import           PPDiff (ColorEnable(..))
import           StrUtils (stripNewline)
import           System.Environment (getEnv)
import           System.Exit (ExitCode(..))
import           System.Posix.IO (stdOutput)
import           System.Posix.Terminal (queryTerminal)
import           System.Process (callProcess, readProcessWithExitCode)

import           Prelude.Compat

shouldUseColorByTerminal :: IO ColorEnable
shouldUseColorByTerminal =
    do  istty <- queryTerminal stdOutput
        return $ if istty then EnableColor else DisableColor

getConflictStyle :: IO String
getConflictStyle =
    do  (exitCode, stdout, _) <- readProcessWithExitCode "git" ["config", "merge.conflictstyle"] stdin
        case exitCode of
            ExitSuccess -> return $ stripNewline stdout
            ExitFailure 1 -> return "unset"
            ExitFailure _ -> E.throwIO exitCode
    where
        stdin = ""

setConflictStyle :: IO ()
setConflictStyle =
    callProcess "git" ["config", "--global", "merge.conflictstyle", "diff3"]

checkConflictStyle :: Options -> IO ()
checkConflictStyle opts =
    do  conflictStyle <- getConflictStyle
        when (conflictStyle /= "diff3") $
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
    | otherwise = return ()
