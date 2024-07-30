{-# LANGUAGE NoImplicitPrelude, OverloadedRecordDot #-}

module Git
    ( StatusLine(..), StatusCode, getStatus, getRootDir, getCdUp, add
    , makeFilesMatchingPrefixes
    ) where

import           Control.Monad (when, filterM)
import           Data.Foldable (asum)
import           Data.List.Split (splitOn)
import           Data.Maybe (mapMaybe)
import           StrUtils ((</>), stripNewline)
import           System.Directory (getCurrentDirectory)
import           System.Exit (ExitCode(..), exitWith)
import           System.FilePath (makeRelative, joinPath, splitPath)
import           System.IO (hPutStr, stderr)
import qualified System.PosixCompat.Files as PosixFiles
import           System.Process (callProcess, readProcess, readProcessWithExitCode)

import           Prelude.Compat

type StatusCode = (Char, Char)

data StatusLine = StatusLine
    { statusCode :: StatusCode
    , statusArgs :: String
    }

parseStatusZ :: [String] -> Either String [StatusLine]
parseStatusZ [""] = Right []
parseStatusZ (('R':' ':dst):_src:rest) =
    -- We don't currently do anything with rename statuses so _src is ignored
    (StatusLine ('R', ' ') dst :) <$> parseStatusZ rest
-- TODO: Which other statuses have two fields?
parseStatusZ ((x:y:' ':arg):rest) = (StatusLine (x, y) arg :) <$> parseStatusZ rest
parseStatusZ part = Left ("Cannot parse status -z part: " <> show part)

getStatus :: IO [StatusLine]
getStatus =
    do
        (resCode, statusZ, statusStderr) <-
            readProcessWithExitCode "git" ["status", "-z"] ""
        when (resCode /= ExitSuccess) $ do
            -- Print git's error message. Usually -
            -- "fatal: Not a git repository (or any of the parent directories): .git"
            hPutStr stderr statusStderr
            exitWith resCode
        case parseStatusZ $ splitOn "\0" statusZ of
            Right res -> pure res
            Left err ->
                do
                    hPutStr stderr err
                    exitWith (ExitFailure 1)

getRootDir :: IO FilePath
getRootDir =
    do
        cwd <- getCurrentDirectory
        relativePath cwd . stripNewline
            <$> readProcess "git" ["rev-parse", "--show-toplevel"] ""

relativePath :: FilePath -> FilePath -> FilePath
relativePath base path
    | rel /= path = rel
    | revRel /= base =
        joinPath $ replicate (length (splitPath revRel)) ".."
    | otherwise = path
    where
        rel = makeRelative base path
        revRel = makeRelative path base

add :: FilePath -> IO ()
add fileName = callProcess "git" ["add", "--", fileName]

-- TODO: Is this different from getRootDir?
getCdUp :: IO FilePath
getCdUp = takeWhile (/= '\0') . stripNewline <$> readProcess "git" ["rev-parse", "--show-cdup"] ""

makeFilesMatchingPrefixes :: IO ([Git.StatusCode] -> IO [FilePath])
makeFilesMatchingPrefixes =
    do
        statusPorcelain <- Git.getStatus
        rootDir <- Git.getRootDir
        let rootRelativeFiles =
                filterM (fmap not . isDirectory) . map (rootDir </>)
        let decode x =
                case reads x of
                [(r, "")] -> r
                _ -> x
        let firstMatchingStatus :: [Git.StatusCode] -> Git.StatusLine -> Maybe String
            firstMatchingStatus statuses =
                fmap decode . asum . traverse matchStatus statuses
        let filesMatchingStatuses :: [Git.StatusCode] -> IO [FilePath]
            filesMatchingStatuses statuses =
                rootRelativeFiles . mapMaybe (firstMatchingStatus statuses)
                $ statusPorcelain
        pure filesMatchingStatuses

matchStatus :: Git.StatusCode -> Git.StatusLine -> Maybe String
matchStatus code line
    | line.statusCode == code = Just line.statusArgs
    | otherwise = Nothing

isDirectory :: FilePath -> IO Bool
isDirectory x = PosixFiles.isDirectory <$> PosixFiles.getFileStatus x
