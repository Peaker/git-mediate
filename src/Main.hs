{-# LANGUAGE NoImplicitPrelude, OverloadedRecordDot, FlexibleContexts #-}

module Main (main) where

import           Conflict (Conflict(..))
import qualified Conflict
import qualified Control.Exception as E
import           Control.Monad (when, unless, filterM)
import           Data.Algorithm.Diff (Diff, PolyDiff(..))
import           Data.Either (rights)
import           Data.Foldable (asum, traverse_)
import           Data.List (isPrefixOf)
import           Data.Maybe (mapMaybe)
import           Environment (checkConflictStyle, openEditor, shouldUseColorByTerminal)
import qualified Opts
import           Opts (Options(..))
import           PPDiff (ppDiff, ColorEnable(..))
import           Resolution (Result(..), NewContent(..), Untabify(..))
import qualified Resolution
import           SideDiff (SideDiff(..), getConflictDiffs, getConflictDiff2s)
import           StrUtils (ensureNewline, stripNewline)
import           System.Directory (renameFile, removeFile, getCurrentDirectory, getPermissions, setPermissions)
import           System.Exit (ExitCode(..), exitWith)
import           System.FilePath ((<.>), makeRelative, joinPath, splitPath)
import qualified System.FilePath as FilePath
import           System.IO (hPutStr, stderr)
import qualified System.PosixCompat.Files as PosixFiles
import           System.Process (callProcess, readProcess, readProcessWithExitCode)

import           Prelude.Compat

-- '>' -> ">>>>>>>"
markerPrefix :: Char -> String
markerPrefix = replicate 7

markerLine :: Char -> String -> String
markerLine c str = markerPrefix c ++ " " ++ str ++ "\n"

gitAdd :: FilePath -> IO ()
gitAdd fileName =
    callProcess "git" ["add", "--", fileName]

trimDiff :: Int -> [Diff a] -> [Diff a]
trimDiff contextLen =
    reverse . f . reverse . f
    where
        f l = drop (length (takeWhile both l) - contextLen) l
        both Both{} = True
        both _ = False

dumpDiffs :: ColorEnable -> Options -> FilePath -> Int -> (Int, Conflict) -> IO ()
dumpDiffs colorEnable opts filePath count (idx, conflict) =
    do
        putStrLn $ unwords ["### Conflict", show idx, "of", show count]
        when opts.shouldDumpDiffs $ traverse_ dumpDiff $ getConflictDiffs conflict
        when opts.shouldDumpDiff2 $ dumpDiff2 $ getConflictDiff2s conflict
    where
        dumpDiff d =
            do  putStrLn $ concat
                    [filePath, ":", show d.marker.lineNo, ":Diff", show d.side, ": ", d.marker.content]
                putStr $ unlines $ map (ppDiff colorEnable) (trimDiff opts.diffsContext d.diff)
        dumpDiff2 (markerA, markerB, d) =
            do  putStrLn $ concat [filePath, ":", show markerA.lineNo, " <->", markerA.content]
                putStrLn $ concat [filePath, ":", show markerB.lineNo, ": ", markerB.content]
                putStr $ unlines $ map (ppDiff colorEnable) d

dumpAndOpenEditor :: ColorEnable -> Options -> FilePath -> [Conflict] -> IO ()
dumpAndOpenEditor colorEnable opts path conflicts =
    do  when (opts.shouldDumpDiffs || opts.shouldDumpDiff2) $
            traverse_ (dumpDiffs colorEnable opts path (length conflicts)) (zip [1..] conflicts)
        openEditor opts path ((Conflict.lineNo . Conflict.sideA . markers . head) conflicts)

overwrite :: FilePath -> String -> IO ()
overwrite fileName content =
    do  oldPermissions <- getPermissions fileName
        renameFile fileName bkup
        writeFile fileName content
        setPermissions fileName oldPermissions
        removeFile bkup
    where
        bkup = fileName <.> "bk"

handleFileResult :: ColorEnable -> Options -> FilePath -> NewContent -> IO ()
handleFileResult colorEnable opts fileName res
    | successes == 0 && allGood =
      do  putStrLn $ fileName ++ ": No conflicts, git-adding"
          gitAdd fileName
    | successes == 0 && reductions == 0 =
      do  putStrLn $ concat
              [ fileName, ": Failed to resolve any of the "
              , show failures, " conflicts" ]
          doDump
    | successes == 0 =
      do  putStrLn $ concat
              [ fileName, ": Reduced ", show reductions, " conflicts"]
          overwrite fileName res.newContent
          doDump
    | otherwise =
      do  putStrLn $ concat
              [ fileName, ": Successfully resolved ", show successes
              , " conflicts (failed to resolve " ++ show (reductions + failures) ++ " conflicts)"
              , if allGood then ", git adding" else ""
              ]
          overwrite fileName res.newContent
          if allGood
              then gitAdd fileName
              else doDump
    where
        allGood = Resolution.fullySuccessful res.result
        doDump =
            dumpAndOpenEditor colorEnable opts fileName
            (rights (Conflict.parse res.newContent))
        Result
            { resolvedSuccessfully = successes
            , reducedConflicts = reductions
            , failedToResolve = failures
            } = res.result

resolve :: ColorEnable -> Options -> FilePath -> IO Result
resolve colorEnable opts fileName =
    do
        resolutions <-
            Resolution.resolveContent (Untabify opts.untabify)
            . Conflict.parse
            <$> readFile fileName
        resolutions.result <$ handleFileResult colorEnable opts fileName resolutions

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
        localContent <- foldMap readFile mLocalTmp
        remoteContent <- foldMap readFile mRemoteTmp
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
    do  marked <-
            any (markerPrefix '<' `isPrefixOf`) . lines <$> readFile path
        unless marked $
            do  putStrLn $ show path ++ " has a delete/modify conflict. Adding conflict markers"
                deleteModifyConflictAddMarkers path

removeFileIfEmpty :: FilePath -> IO ()
removeFileIfEmpty path =
    do  isEmpty <- null <$> readFile path
        when isEmpty $
            do  removeFile path
                callProcess "git" ["add", "-u", "--", path]

type StatusCode = (Char, Char)

data StatusLine = StatusLine
    { statusCode :: StatusCode
    , statusArgs :: String
    }

parseStatusPorcelainLine :: String -> Either String StatusLine
parseStatusPorcelainLine (x:y:' ':rest) = Right (StatusLine (x, y) rest)
parseStatusPorcelainLine line = Left ("Cannot parse status line: " <> show line)

getStatusPorcelain :: IO [StatusLine]
getStatusPorcelain =
    do  (resCode, statusPorcelain, statusStderr) <-
            readProcessWithExitCode "git" ["status", "--porcelain"] ""
        when (resCode /= ExitSuccess) $ do
            -- Print git's error message. Usually -
            -- "fatal: Not a git repository (or any of the parent directories): .git"
            hPutStr stderr statusStderr
            exitWith resCode
        case traverse parseStatusPorcelainLine . lines $ statusPorcelain of
            Right res -> pure res
            Left err ->
                do
                    hPutStr stderr err
                    exitWith (ExitFailure 1)

getGitRootDir :: IO FilePath
getGitRootDir =
  do  cwd <- getCurrentDirectory
      relativePath cwd . stripNewline <$>
          readProcess "git" ["rev-parse", "--show-toplevel"] ""

matchStatus :: StatusCode -> StatusLine -> Maybe String
matchStatus code line
    | statusCode line == code = Just (statusArgs line)
    | otherwise = Nothing

makeFilesMatchingPrefixes :: IO ([StatusCode] -> IO [FilePath])
makeFilesMatchingPrefixes =
  do  statusPorcelain <- getStatusPorcelain
      rootDir <- getGitRootDir
      let rootRelativeFiles =
              filterM (fmap not . isDirectory) . map (rootDir </>)
      let decode x =
              case reads x of
              [(r, "")] -> r
              _ -> x
      let firstMatchingStatus :: [StatusCode] -> StatusLine -> Maybe String
          firstMatchingStatus statuses =
              fmap decode . asum . traverse matchStatus statuses
      let filesMatchingStatuses :: [StatusCode] -> IO [FilePath]
          filesMatchingStatuses statuses =
              rootRelativeFiles . mapMaybe (firstMatchingStatus statuses)
              $ statusPorcelain
      pure filesMatchingStatuses

statusCodes :: Char -> Char -> [StatusCode]
statusCodes x y
    | x == y = [(x, y)]
    | otherwise = [(x, y), (y, x)]

mediateAll :: ColorEnable -> Options -> IO Result
mediateAll colorEnable opts =
  do  filesMatchingPrefixes <- makeFilesMatchingPrefixes

-- from git-diff manpage:
-- Added (A), Copied (C), Deleted (D), Modified (M), Renamed (R),
-- have their type (i.e. regular file, symlink, submodule, ...) changed (T),
-- are Unmerged (U), are Unknown (X), or have had their pairing Broken (B)

      deleteModifyConflicts <- filesMatchingPrefixes (statusCodes 'D' 'U')

      traverse_ deleteModifyConflictHandle deleteModifyConflicts

      res <-
          filesMatchingPrefixes
          ([('U', 'U'), ('A', 'A'), ('D', 'A'), ('D', 'U')] >>= uncurry statusCodes)
          >>= foldMap (resolve colorEnable opts)

      -- Heuristically delete files that were remove/modify conflict
      -- and ended up with empty content
      traverse_ removeFileIfEmpty deleteModifyConflicts
      pure res

exitCodeOf :: Result -> ExitCode
exitCodeOf res
    | Resolution.fullySuccessful res = ExitSuccess
    | otherwise = ExitFailure 111

exitProcess :: Result -> IO ()
exitProcess = exitWith . exitCodeOf

main :: IO ()
main =
  do  opts <- Opts.getOpts
      colorEnable <- maybe shouldUseColorByTerminal pure opts.shouldUseColor
      checkConflictStyle opts
      case opts.mergeSpecificFile of
          Nothing -> mediateAll colorEnable opts
          Just path -> resolve colorEnable opts path
          >>= exitProcess
