{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import System.Directory (renameFile)
import System.Environment (getArgs)
import System.Exit
import System.FilePath
import System.Process
import qualified System.IO as IO

data Conflict = Conflict
  { _markerA    :: String -- <<<<<<<....
  , _markerBase :: String -- |||||||....
  , _markerB    :: String -- =======....
  , _markerEnd  :: String -- >>>>>>>....
  , _linesA     :: [String]
  , _linesBase  :: [String]
  , _linesB     :: [String]
  } deriving (Show)

prettyConflict :: Conflict -> String
prettyConflict (Conflict markerA markerBase markerB markerEnd linesA linesBase linesB) =
  unlines $ concat
  [ markerA    : linesA
  , markerBase : linesBase
  , markerB    : linesB
  , [markerEnd]
  ]

resolveConflict :: Conflict -> Maybe String
resolveConflict (Conflict _ _ _ _ a base b)
  | a == base = Just $ unlines b
  | b == base = Just $ unlines a
  | otherwise = Nothing

breakUpToMarker :: (Eq a, MonadState [[a]] m) => a -> m [[a]]
breakUpToMarker c = state (break (replicate 7 c `isPrefixOf`))

readHead :: MonadState [a] m => m (Maybe a)
readHead = state f
  where
    f [] = (Nothing, [])
    f (l:ls) = (Just l, ls)

readUpToMarker :: (Eq a, MonadState [[a]] m) => a -> m ([[a]], Maybe [a])
readUpToMarker c =
  do
    ls <- breakUpToMarker c
    mHead <- readHead
    return (ls, mHead)

parseConflict :: MonadState [String] m => String -> m Conflict
parseConflict markerA = do
  (linesA   , Just markerBase) <- readUpToMarker '|'
  (linesBase, Just markerB) <- readUpToMarker '='
  (linesB   , Just markerEnd) <- readUpToMarker '>'
  return Conflict
    { _markerA    = markerA
    , _markerBase = markerBase
    , _markerB    = markerB
    , _markerEnd  = markerEnd
    , _linesA     = linesA
    , _linesB     = linesB
    , _linesBase  = linesBase
    }

parseConflicts :: String -> [Either String Conflict]
parseConflicts input =
  snd $ runWriter $ evalStateT loop (lines input)
  where
    loop =
      do
        (ls, mMarkerA) <- readUpToMarker '<'
        tell $ map Left ls
        case mMarkerA of
          Nothing -> return ()
          Just markerA ->
            do
              tell . return . Right =<< parseConflict markerA
              loop

data NewContent = NewContent
  { _resolvedSuccessfully :: Int
  , _failedToResolve :: Int
  , _newContent :: String
  }

resolveContent :: [Either String Conflict] -> NewContent
resolveContent = asResult . mconcat . map go
  where
    asResult (Sum successes, Sum failures, newContent) = NewContent
      { _resolvedSuccessfully = successes
      , _failedToResolve = failures
      , _newContent = newContent
      }
    go (Left line) = (Sum 0, Sum 0, unlines [line])
    go (Right conflict) =
      case resolveConflict conflict of
      Nothing -> (Sum 0, Sum 1, prettyConflict conflict)
      Just trivialLines -> (Sum 1, Sum 0, trivialLines)

verifyExitCode :: String -> ExitCode -> IO ()
verifyExitCode cmd exitCode =
  case exitCode of
    ExitFailure i ->
      putStrLn $
      "Failed to execute: " ++ cmd ++ " (" ++ show i ++ ")"
    ExitSuccess -> return ()

gitAdd :: FilePath -> IO ()
gitAdd fileName = verifyExitCode cmd =<< system cmd
  where
    cmd = "git add -- " ++ show fileName

resolve :: FilePath -> IO ()
resolve fileName =
  do
    content <- parseConflicts <$> readFile fileName
    case resolveContent content of
      NewContent successes failures newContent
        | successes == 0 &&
          failures == 0 -> do
            putStrLn $ fileName ++ ": No conflicts, git-adding"
            gitAdd fileName
        | successes == 0 -> putStrLn $ concat
            [ fileName, ": Failed to resolve any of the "
            , show failures, " conflicts" ]
        | otherwise ->
          do
            putStrLn $ concat
              [ fileName, ": Successfully resolved ", show successes
              , " conflicts (failed to resolve " ++ show failures ++ " conflicts)"
              , if failures == 0 then ", git adding" else ""
              ]
            renameFile fileName (fileName <.> "bk")
            writeFile fileName newContent
            when (failures == 0) $ gitAdd fileName

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> return ()
    _ -> fail "No args acceptable"
  let stdin = ""
  (exitCode, stdout, stderr) <-
    readProcessWithExitCode "git" ["status", "--porcelain"] stdin
  IO.hPutStrLn IO.stderr stderr
  verifyExitCode "git status --porcelain" exitCode
  let fileNames =
          map ((!! 1) . words) $ filter ("UU" `isPrefixOf`) $ lines stdout
  mapM_ resolve fileNames
