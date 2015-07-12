{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Main (main) where

import           Control.Applicative ((<$>))
import qualified Control.Exception as E
import           Control.Monad (when, unless)
import           Control.Monad.State (MonadState, state, evalStateT)
import           Control.Monad.Writer (runWriter, tell)
import           Data.Algorithm.Diff (Diff, getDiff)
import           Data.List (isPrefixOf, isSuffixOf)
import           Data.Maybe (mapMaybe)
import           Data.Monoid (Monoid(..))
import qualified Data.Monoid as Monoid
import           PPDiff (ppDiff, ColorEnable(..))
import           System.Directory (renameFile, removeFile, getCurrentDirectory)
import           System.Environment (getProgName, getArgs, getEnv)
import           System.Exit (ExitCode(..))
import           System.FilePath ((<.>), makeRelative, joinPath, splitPath)
import qualified System.FilePath as FilePath
import           System.Posix.IO (stdOutput)
import           System.Posix.Terminal (queryTerminal)
import           System.Process (callProcess, readProcess, readProcessWithExitCode)

data Side = A | B
  deriving (Eq, Ord, Show)
type LineNo = Int

data Conflict = Conflict
  { cMarkerA    :: (LineNo, String) -- <<<<<<<....
  , cMarkerBase :: (LineNo, String) -- |||||||....
  , cMarkerB    :: (LineNo, String) -- =======....
  , cMarkerEnd  :: (LineNo, String) -- >>>>>>>....
  , cLinesA     :: [String]
  , cLinesBase  :: [String]
  , cLinesB     :: [String]
  } deriving (Show)

prettyConflict :: Conflict -> String
prettyConflict Conflict {..} =
  unlines $ concat
  [ snd cMarkerA    : cLinesA
  , snd cMarkerBase : cLinesBase
  , snd cMarkerB    : cLinesB
  , [snd cMarkerEnd]
  ]

resolveConflict :: Conflict -> Maybe String
resolveConflict Conflict{..}
  | cLinesA == cLinesBase = Just $ unlines cLinesB
  | cLinesB == cLinesBase = Just $ unlines cLinesA
  | cLinesA == cLinesB = Just $ unlines cLinesA
  | otherwise = Nothing

breakUpToMarker :: MonadState [(LineNo, String)] m => Char -> m [(LineNo, String)]
breakUpToMarker c = state (break ((replicate 7 c `isPrefixOf`) . snd))

readHead :: MonadState [a] m => m (Maybe a)
readHead = state f
  where
    f [] = (Nothing, [])
    f (l:ls) = (Just l, ls)

readUpToMarker :: MonadState [(LineNo, String)] m => Char -> m ([(LineNo, String)], Maybe (LineNo, String))
readUpToMarker c =
  do
    ls <- breakUpToMarker c
    mHead <- readHead
    return (ls, mHead)

parseConflict :: MonadState [(LineNo, String)] m => (LineNo, String) -> m Conflict
parseConflict markerA = do
  (linesA   , Just markerBase) <- readUpToMarker '|'
  (linesBase, Just markerB)     <- readUpToMarker '='
  (linesB   , Just markerEnd)  <- readUpToMarker '>'
  return Conflict
    { cMarkerA    = markerA
    , cMarkerBase = markerBase
    , cMarkerB    = markerB
    , cMarkerEnd  = markerEnd
    , cLinesA     = map snd linesA
    , cLinesB     = map snd linesB
    , cLinesBase  = map snd linesBase
    }

parseConflicts :: String -> [Either String Conflict]
parseConflicts input =
  snd $ runWriter $ evalStateT loop (zip [1..] (lines input))
  where
    loop =
      do
        (ls, mMarkerA) <- readUpToMarker '<'
        tell $ map (Left . snd) ls
        case mMarkerA of
          Nothing -> return ()
          Just markerA ->
            do
              tell . return . Right =<< parseConflict markerA
              loop

type SideDiff = (Side, (LineNo, String), [Diff String])

data NewContent = NewContent
  { _resolvedSuccessfully :: Int
  , _failedToResolve :: Int
  , _newContent :: String
  , _diffs :: [SideDiff]
  }

getConflictDiffs :: Conflict -> [SideDiff]
getConflictDiffs Conflict{..} =
    [ (A, cMarkerA, getDiff cLinesBase cLinesA) | not (null cLinesA) ] ++
    [ (B, (fst cMarkerB, snd cMarkerEnd), getDiff cLinesBase cLinesB) | not (null cLinesB) ]

resolveContent :: [Either String Conflict] -> NewContent
resolveContent = asResult . mconcat . map go
  where
    asResult (Monoid.Sum successes, Monoid.Sum failures, newContent, diffs) = NewContent
      { _resolvedSuccessfully = successes
      , _failedToResolve = failures
      , _newContent = newContent
      , _diffs = diffs
      }
    go (Left line) = (Monoid.Sum 0, Monoid.Sum 0, unlines [line], [])
    go (Right conflict) =
      case resolveConflict conflict of
      Nothing ->
        ( Monoid.Sum 0, Monoid.Sum 1, prettyConflict conflict
        , getConflictDiffs conflict
        )
      Just trivialLines -> (Monoid.Sum 1, Monoid.Sum 0, trivialLines, [])

gitAdd :: FilePath -> IO ()
gitAdd fileName =
  callProcess "git" ["add", "--", fileName]

data Options = Options
  { shouldUseEditor :: Bool
  , shouldDumpDiffs :: Bool
  , shouldUseColor :: Maybe ColorEnable
  , shouldSetConflictStyle :: Bool
  }
instance Monoid Options where
  mempty = Options False False Nothing False
  Options oe0 od0 oc0 os0 `mappend` Options oe1 od1 oc1 os1 =
    Options
    (combineBool oe0 oe1 "-e")
    (combineBool od0 od1 "-d")
    (combineMaybe oc0 oc1 "-c or -C")
    (os0 || os1)
    where
      err flag = error $ "Multiple " ++ flag ++ " flags used"
      combineMaybe (Just _) (Just _) flag = err flag
      combineMaybe Nothing Nothing _ = Nothing
      combineMaybe (Just x) Nothing _ = Just x
      combineMaybe Nothing (Just y) _ = Just y
      combineBool True True flag = err flag
      combineBool x y _ = x || y

getOpts :: [String] -> IO Options
getOpts = fmap mconcat . mapM parseArg
  where
    parseArg "-e" = return mempty { shouldUseEditor = True }
    parseArg "-d" = return mempty { shouldDumpDiffs = True }
    parseArg "-c" = return mempty { shouldUseColor = Just EnableColor }
    parseArg "-C" = return mempty { shouldUseColor = Just DisableColor }
    parseArg "-s" = return mempty { shouldSetConflictStyle = True }
    parseArg arg =
      do  prog <- getProgName
          putStr $ unlines
            [ "Usage: " ++ prog ++ " [-e] [-d] [-c] [-C] [-s]"
            , ""
            , "-e    Execute $EDITOR for each conflicted file that remains conflicted"
            , "-d    Dump the left/right diffs from base in each conflict remaining"
            , "-c    Enable color"
            , "-C    Disable color"
            , "-s    Configure git's global merge.conflictstyle to diff3 if needed"
            ]
          fail $ "Unknown argument: " ++ show arg

openEditor :: Options -> FilePath -> IO ()
openEditor opts path
  | shouldUseEditor opts =
    do
      editor <- getEnv "EDITOR"
      callProcess editor [path]
  | otherwise = return ()

dumpDiffs :: ColorEnable -> Options -> FilePath -> [SideDiff] -> IO ()
dumpDiffs colorEnable opts filePath diffs
  | shouldDumpDiffs opts = mapM_ dumpDiff diffs
  | otherwise = return ()
  where
    dumpDiff (side, (lineNo, marker), diff) =
      do
        putStrLn $ concat
            [filePath, ":", show lineNo, ":Diff", show side, ": ", marker]
        putStr $ unlines $ map (ppDiff colorEnable) diff

dumpAndOpenEditor :: ColorEnable -> Options -> FilePath -> [SideDiff] -> IO ()
dumpAndOpenEditor colorEnable opts path diffs =
  do
    dumpDiffs colorEnable opts path diffs
    openEditor opts path

resolve :: ColorEnable -> Options -> FilePath -> IO ()
resolve colorEnable opts fileName =
  do
    content <- parseConflicts <$> readFile fileName
    case resolveContent content of
      NewContent successes failures newContent diffs
        | successes == 0 &&
          failures == 0 -> do
            putStrLn $ fileName ++ ": No conflicts, git-adding"
            gitAdd fileName
        | successes == 0 -> do
            putStrLn $ concat
              [ fileName, ": Failed to resolve any of the "
              , show failures, " conflicts" ]
            dumpAndOpenEditor colorEnable opts fileName diffs
        | otherwise ->
          do
            putStrLn $ concat
              [ fileName, ": Successfully resolved ", show successes
              , " conflicts (failed to resolve " ++ show failures ++ " conflicts)"
              , if failures == 0 then ", git adding" else ""
              ]
            let bkup = fileName <.> "bk"
            renameFile fileName bkup
            writeFile fileName newContent
            removeFile bkup
            if failures == 0
              then gitAdd fileName
              else dumpAndOpenEditor colorEnable opts fileName diffs

stripNewline :: String -> String
stripNewline x
    | "\n" `isSuffixOf` x = init x
    | otherwise = x

shouldUseColorByTerminal :: IO ColorEnable
shouldUseColorByTerminal =
    do  istty <- queryTerminal stdOutput
        return $ if istty then EnableColor else DisableColor

unprefix :: Eq a => [a] -> [a] -> Maybe [a]
unprefix prefix str
    | prefix `isPrefixOf` str = Just (drop (length prefix) str)
    | otherwise = Nothing

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

main :: IO ()
main =
  do  opts <- getOpts =<< getArgs
      colorEnable <-
          case shouldUseColor opts of
              Nothing -> shouldUseColorByTerminal
              Just colorEnable -> return colorEnable
      checkConflictStyle opts
      let stdin = ""
      statusPorcelain <- readProcess "git" ["status", "--porcelain"] stdin
      let rootRelativeFileNames =
              mapMaybe (unprefix "UU ") $ lines statusPorcelain
      cwd <- getCurrentDirectory
      rootDir <-
          relativePath cwd . stripNewline <$>
          readProcess "git" ["rev-parse", "--show-toplevel"] stdin
      mapM_ (resolve colorEnable opts .
             (rootDir </>)) rootRelativeFileNames
