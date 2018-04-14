{-# LANGUAGE NoImplicitPrelude, LambdaCase #-}

module Main (main) where

import           Conflict (Conflict(..), prettyConflict, parseConflicts, markerPrefix)
import qualified Control.Exception as E
import           Control.Monad (when, unless, filterM)
import           Data.Foldable (asum, traverse_)
import           Data.List (isPrefixOf)
import           Data.Maybe (mapMaybe)
import qualified Data.Monoid as Monoid
import           Environment (checkConflictStyle, openEditor, shouldUseColorByTerminal)
import qualified Opts
import           Opts (Options(..))
import           PPDiff (ppDiff, ColorEnable(..))
import           Resolution (Resolution(..), resolveConflict)
import           SideDiff (getConflictDiffs, getConflictDiff2s)
import           StrUtils (ensureNewline, stripNewline, unprefix)
import           System.Directory (renameFile, removeFile, getCurrentDirectory)
import           System.Exit (ExitCode(..), exitWith)
import           System.FilePath ((<.>), makeRelative, joinPath, splitPath)
import qualified System.FilePath as FilePath
import           System.IO (hPutStr, stderr)
import qualified System.PosixCompat.Files as PosixFiles
import           System.Process (callProcess, readProcess, readProcessWithExitCode)

import           Prelude.Compat

markerLine :: Char -> String -> String
markerLine c str = markerPrefix c ++ " " ++ str ++ "\n"

data NewContent = NewContent
    { _resolvedSuccessfully :: Int
    , _reducedConflicts :: Int
    , _failedToResolve :: Int
    , _newContent :: String
    }

resolveContent :: [Either String Conflict] -> NewContent
resolveContent =
    asResult . mconcat . map go
    where
        asResult (Monoid.Sum successes, Monoid.Sum reductions, Monoid.Sum failures, newContent) =
            NewContent
            { _resolvedSuccessfully = successes
            , _reducedConflicts = reductions
            , _failedToResolve = failures
            , _newContent = newContent
            }
        go (Left line) = (Monoid.Sum 0, Monoid.Sum 0, Monoid.Sum 0, unlines [line])
        go (Right conflict) =
            case resolveConflict conflict of
            NoResolution -> (Monoid.Sum 0, Monoid.Sum 0, Monoid.Sum 1, prettyConflict conflict)
            Resolution trivialLines -> (Monoid.Sum 1, Monoid.Sum 0, Monoid.Sum 0, trivialLines)
            PartialResolution newLines -> (Monoid.Sum 0, Monoid.Sum 1, Monoid.Sum 0, newLines)

gitAdd :: FilePath -> IO ()
gitAdd fileName =
    callProcess "git" ["add", "--", fileName]

dumpDiffs :: ColorEnable -> Options -> FilePath -> Int -> (Int, Conflict) -> IO ()
dumpDiffs colorEnable opts filePath count (idx, conflict) =
    do
        putStrLn $ unwords ["### Conflict", show idx, "of", show count]
        when (shouldDumpDiffs opts) $ mapM_ dumpDiff $ getConflictDiffs conflict
        when (shouldDumpDiff2 opts) $ dumpDiff2 $ getConflictDiff2s conflict
    where
        dumpDiff (side, (lineNo, marker), diff) =
            do  putStrLn $ concat
                    [filePath, ":", show lineNo, ":Diff", show side, ": ", marker]
                putStr $ unlines $ map (ppDiff colorEnable) diff
        dumpDiff2 ((lineNoA, markerA), (lineNoB, markerB), diff) =
            do  putStrLn $ concat [filePath, ":", show lineNoA, " <->", markerA]
                putStrLn $ concat [filePath, ":", show lineNoB, ": ", markerB]
                putStr $ unlines $ map (ppDiff colorEnable) diff

dumpAndOpenEditor :: ColorEnable -> Options -> FilePath -> [Conflict] -> IO ()
dumpAndOpenEditor colorEnable opts path conflicts =
    do  when (shouldDumpDiffs opts || shouldDumpDiff2 opts) $
            mapM_ (dumpDiffs colorEnable opts path (length conflicts)) (zip [1..] conflicts)
        openEditor opts path

overwrite :: FilePath -> String -> IO ()
overwrite fileName newContent =
    do  renameFile fileName bkup
        writeFile fileName newContent
        removeFile bkup
    where
        bkup = fileName <.> "bk"

resolve :: ColorEnable -> Options -> FilePath -> IO ()
resolve colorEnable opts fileName =
    resolveContent . parseConflicts <$> readFile fileName
    >>= \case
    NewContent successes reductions failures newContent
        | successes == 0 && allGood ->
          do  putStrLn $ fileName ++ ": No conflicts, git-adding"
              gitAdd fileName
        | successes == 0 && reductions == 0 ->
          do  putStrLn $ concat
                  [ fileName, ": Failed to resolve any of the "
                  , show failures, " conflicts" ]
              doDump
        | successes == 0 ->
          do  putStrLn $ concat
                  [ fileName, ": Reduced ", show reductions, " conflicts"]
              overwrite fileName newContent
              doDump
        | otherwise ->
          do  putStrLn $ concat
                  [ fileName, ": Successfully resolved ", show successes
                  , " conflicts (failed to resolve " ++ show (reductions + failures) ++ " conflicts)"
                  , if allGood then ", git adding" else ""
                  ]
              overwrite fileName newContent
              if allGood
                  then gitAdd fileName
                  else doDump
        where
            allGood = failures == 0 && reductions == 0
            doDump =
                dumpAndOpenEditor colorEnable opts fileName
                [ conflict | Right conflict <- parseConflicts newContent ]

relativePath :: FilePath -> FilePath -> FilePath
relativePath base path
    | rel /= path = rel
    | revRel /= base =
          joinPath $ replicate (length (splitPath revRel)) ".."
    | otherwise = path
    where
        rel = makeRelative base path
        revRel = makeRelative path base

(</>) :: FilePath -> FilePath -> FilePath
"." </> p = p
d </> p = d FilePath.</> p

isDirectory :: FilePath -> IO Bool
isDirectory x = PosixFiles.isDirectory <$> PosixFiles.getFileStatus x

withAllStageFiles ::
    FilePath -> (FilePath -> Maybe FilePath -> Maybe FilePath -> IO b) -> IO b
withAllStageFiles path action =
    do  let stdin = ""
        [baseTmpRaw, localTmpRaw, remoteTmpRaw] <-
            take 3 . words <$>
            readProcess "git" ["checkout-index", "--stage=all", "--", path] stdin
        cdup <-
            takeWhile (/= '\0') . stripNewline <$>
            readProcess "git" ["rev-parse", "--show-cdup"] stdin
        let maybePath "." = Nothing
            maybePath p = Just (cdup </> p)
        let mLocalTmp = maybePath localTmpRaw
            mRemoteTmp = maybePath remoteTmpRaw
            baseTmp = cdup </> baseTmpRaw
        action baseTmp mLocalTmp mRemoteTmp
            `E.finally`
            do  removeFile baseTmp
                traverse_ removeFile mLocalTmp
                traverse_ removeFile mRemoteTmp

deleteModifyConflictAddMarkers :: FilePath -> IO ()
deleteModifyConflictAddMarkers path =
    withAllStageFiles path $ \baseTmp mLocalTmp mRemoteTmp ->
    do  baseContent <- readFile baseTmp
        localContent <- maybe (return "") readFile mLocalTmp
        remoteContent <- maybe (return "") readFile mRemoteTmp
        overwrite path $
            concat
            [ markerLine '<' "LOCAL"
            , ensureNewline localContent
            , markerLine '|' "BASE"
            , ensureNewline baseContent
            , markerLine '=' ""
            , ensureNewline remoteContent
            , markerLine '>' "REMOTE"
            ]

deleteModifyConflictHandle :: FilePath -> IO ()
deleteModifyConflictHandle path =
    do  marked <- any (markerPrefix '<' `isPrefixOf`) . lines <$> readFile path
        unless marked $
            do  putStrLn $ show path ++ " has a delete/modify conflict. Adding conflict markers"
                deleteModifyConflictAddMarkers path

removeFileIfEmpty :: FilePath -> IO ()
removeFileIfEmpty path =
    do  isEmpty <- null <$> readFile path
        when isEmpty $
            do  removeFile path
                callProcess "git" ["add", "-u", "--", path]

getStatusPorcelain :: IO String
getStatusPorcelain =
    do  (statusCode, statusPorcelain, statusStderr) <-
            readProcessWithExitCode "git" ["status", "--porcelain"] ""
        when (statusCode /= ExitSuccess) $ do
            -- Print git's error message. Usually -
            -- "fatal: Not a git repository (or any of the parent directories): .git"
            hPutStr stderr statusStderr
            exitWith statusCode
        return statusPorcelain

main :: IO ()
main =
  do  opts <- Opts.getOpts
      colorEnable <-
          case shouldUseColor opts of
              Nothing -> shouldUseColorByTerminal
              Just colorEnable -> return colorEnable
      checkConflictStyle opts
      statusPorcelain <- getStatusPorcelain
      cwd <- getCurrentDirectory
      rootDir <-
          relativePath cwd . stripNewline <$>
          readProcess "git" ["rev-parse", "--show-toplevel"] ""
      let rootRelativeFiles =
              filterM (fmap not . isDirectory) . map (rootDir </>)
      let firstMatchingPrefix :: [String] -> String -> Maybe String
          firstMatchingPrefix prefixes =
              asum . traverse unprefix prefixes
      let filesMatchingPrefixes :: [String] -> IO [FilePath]
          filesMatchingPrefixes prefixes =
              rootRelativeFiles . mapMaybe (firstMatchingPrefix prefixes)
              $ lines statusPorcelain

-- from git-diff manpage:
-- Added (A), Copied (C), Deleted (D), Modified (M), Renamed (R),
-- have their type (i.e. regular file, symlink, submodule, ...) changed (T),
-- are Unmerged (U), are Unknown (X), or have had their pairing Broken (B)

      deleteModifyConflicts <- filesMatchingPrefixes ["DU ", "UD "]

      mapM_ deleteModifyConflictHandle deleteModifyConflicts

      filesMatchingPrefixes ["UU ", "AA ", "DA ", "AD ", "DU ", "UD "]
          >>= mapM_ (resolve colorEnable opts)

      -- Heuristically delete files that were remove/modify conflict
      -- and ended up with empty content
      mapM_ removeFileIfEmpty deleteModifyConflicts
