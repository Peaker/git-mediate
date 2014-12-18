{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import PPDiff (ppDiff)
import System.Directory (renameFile)
import System.Environment (getProgName, getArgs, getEnv)
import System.FilePath
import System.Process
import Data.Algorithm.Diff (Diff, getDiff)

data Side = A | B
  deriving (Eq, Ord, Show)
type LineNo = Int

data Conflict = Conflict
  { _lineNo     :: LineNo
  , _markerA    :: String -- <<<<<<<....
  , _markerBase :: String -- |||||||....
  , _markerB    :: String -- =======....
  , _markerEnd  :: String -- >>>>>>>....
  , _linesA     :: [String]
  , _linesBase  :: [String]
  , _linesB     :: [String]
  } deriving (Show)

prettyConflict :: Conflict -> String
prettyConflict (Conflict _ markerA markerBase markerB markerEnd linesA linesBase linesB) =
  unlines $ concat
  [ markerA    : linesA
  , markerBase : linesBase
  , markerB    : linesB
  , [markerEnd]
  ]

resolveConflict :: Conflict -> Maybe String
resolveConflict Conflict{..}
  | _linesA == _linesBase = Just $ unlines _linesB
  | _linesB == _linesBase = Just $ unlines _linesA
  | _linesA == _linesB = Just $ unlines _linesA
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

parseConflict :: MonadState [(LineNo, String)] m => LineNo -> String -> m Conflict
parseConflict lineNo markerA = do
  (linesA   , Just (_, markerBase)) <- readUpToMarker '|'
  (linesBase, Just (_, markerB)) <- readUpToMarker '='
  (linesB   , Just (_, markerEnd)) <- readUpToMarker '>'
  return Conflict
    { _lineNo     = lineNo
    , _markerA    = markerA
    , _markerBase = markerBase
    , _markerB    = markerB
    , _markerEnd  = markerEnd
    , _linesA     = map snd linesA
    , _linesB     = map snd linesB
    , _linesBase  = map snd linesBase
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
          Just (lineNo, markerA) ->
            do
              tell . return . Right =<< parseConflict lineNo markerA
              loop

type SideDiff = (Side, LineNo, [Diff String])

data NewContent = NewContent
  { _resolvedSuccessfully :: Int
  , _failedToResolve :: Int
  , _newContent :: String
  , _diffs :: [SideDiff]
  }

getConflictDiffs :: Conflict -> [SideDiff]
getConflictDiffs Conflict{..} =
    [ (A, _lineNo, getDiff _linesBase _linesA) | not (null _linesA) ] ++
    [ (B, _lineNo, getDiff _linesBase _linesB) | not (null _linesB) ]

resolveContent :: [Either String Conflict] -> NewContent
resolveContent = asResult . mconcat . map go
  where
    asResult (Sum successes, Sum failures, newContent, diffs) = NewContent
      { _resolvedSuccessfully = successes
      , _failedToResolve = failures
      , _newContent = newContent
      , _diffs = diffs
      }
    go (Left line) = (Sum 0, Sum 0, unlines [line], [])
    go (Right conflict) =
      case resolveConflict conflict of
      Nothing ->
        ( Sum 0, Sum 1, prettyConflict conflict
        , getConflictDiffs conflict
        )
      Just trivialLines -> (Sum 1, Sum 0, trivialLines, [])

gitAdd :: FilePath -> IO ()
gitAdd fileName =
  callProcess "git" ["add", "--", fileName]

data Options = Options
  { shouldUseEditor :: Bool
  , shouldDumpDiffs :: Bool
  }
instance Monoid Options where
  mempty = Options False False
  Options a0 b0 `mappend` Options a1 b1 =
    Options (combineBool a0 a1 "-e") (combineBool b0 b1 "-d")
    where
      combineBool True True flag = error $ "Multiple " ++ flag ++ " flags used"
      combineBool x y _ = x || y

openEditor :: Options -> FilePath -> IO ()
openEditor opts path
  | shouldUseEditor opts =
    do
      editor <- getEnv "EDITOR"
      callProcess editor [path]
  | otherwise = return ()

dumpDiffs :: Options -> FilePath -> [SideDiff] -> IO ()
dumpDiffs opts filePath diffs
  | shouldDumpDiffs opts = mapM_ dumpDiff diffs
  | otherwise = return ()
  where
    dumpDiff (side, lineNo, diff) =
      do
        putStrLn $ filePath ++ ":" ++ show lineNo ++ ":Diff" ++ show side
        putStr $ unlines $ map ppDiff diff

dumpAndOpenEditor :: Options -> FilePath -> [SideDiff] -> IO ()
dumpAndOpenEditor opts path diffs =
  do
    dumpDiffs opts path diffs
    openEditor opts path

resolve :: Options -> FilePath -> IO ()
resolve opts fileName =
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
            dumpAndOpenEditor opts fileName diffs
        | otherwise ->
          do
            putStrLn $ concat
              [ fileName, ": Successfully resolved ", show successes
              , " conflicts (failed to resolve " ++ show failures ++ " conflicts)"
              , if failures == 0 then ", git adding" else ""
              ]
            renameFile fileName (fileName <.> "bk")
            writeFile fileName newContent
            if failures == 0
              then gitAdd fileName
              else dumpAndOpenEditor opts fileName diffs

stripNewline :: String -> String
stripNewline x
    | "\n" `isSuffixOf` x = init x
    | otherwise = x

getOpts :: [String] -> IO Options
getOpts = fmap mconcat . mapM parseArg
  where
    parseArg "-e" = return mempty { shouldUseEditor = True }
    parseArg "-d" = return mempty { shouldDumpDiffs = True }
    parseArg _ =
      do  prog <- getProgName
          fail $ unlines
            [ "Usage: " ++ prog ++ " [-e] [-d]"
            , ""
            , "-e    Execute $EDITOR for each conflicted file that remains conflicted"
            , "-d    Dump the left/right diffs from base in each conflict remaining"
            ]

main :: IO ()
main = do
  opts <- getOpts =<< getArgs
  let stdin = ""
  statusPorcelain <- readProcess "git" ["status", "--porcelain"] stdin
  let rootRelativeFileNames =
          map ((!! 1) . words) $ filter ("UU" `isPrefixOf`) $ lines statusPorcelain
  rootDir <- stripNewline <$> readProcess "git" ["rev-parse", "--show-toplevel"] stdin
  mapM_ (resolve opts . (rootDir </>)) rootRelativeFileNames
