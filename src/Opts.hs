{-# LANGUAGE LambdaCase #-}
-- | Option parser

module Opts
    ( Options(..)
    , getOpts
    ) where

import           Control.Applicative (Alternative(..))
import qualified Options.Applicative as O
import           PPDiff (ColorEnable(..))
import           System.Exit (exitSuccess)
import           Version (versionString)

data Options = Options
    { shouldUseEditor :: Bool
    , shouldDumpDiffs :: Bool
    , shouldDumpDiff2 :: Bool
    , shouldUseColor :: Maybe ColorEnable
    , shouldSetConflictStyle :: Bool
    , untabify :: Maybe Int
    , mergeSpecificFile :: Maybe FilePath
    }

data CmdArgs = CmdVersion | CmdOptions Options

parser :: O.Parser CmdArgs
parser =
    O.flag' CmdVersion (O.long "version" <> O.help "Print the version and quit")
    <|> CmdOptions
        <$> ( Options
            <$> O.switch
                ( O.long "editor" <> O.short 'e'
                  <> O.help "Execute $EDITOR for each conflicted file that remains conflicted"
                )
            <*> O.switch
                ( O.long "diff" <> O.short 'd'
                  <> O.help "Dump the left/right diffs from base in each conflict remaining"
                )
            <*> O.switch
                ( O.long "diff2" <> O.short '2'
                  <> O.help "Dump the diff between left and right in each conflict remaining"
                )
            <*> ( O.flag' (Just EnableColor)
                  (O.long "color" <> O.short 'c' <> O.help "Enable color")
                  <|> O.flag' (Just DisableColor)
                      (O.long "no-color" <> O.short 'C' <> O.help "Disable color")
                  <|> pure Nothing
                )
            <*> O.switch
                ( O.long "style" <> O.short 's'
                  <> O.help "Configure git's global merge.conflictstyle to diff3 if needed"
                )
            <*> O.optional
                ( O.option O.auto
                    ( O.long "untabify" <> O.metavar "TABSIZE"
                        <> O.help "Convert tabs to the spaces at the tab stops for the given tab size"
                    )
                )
            <*> ( ( Just <$>
                    O.strOption
                    (O.long "merge-file" <> O.short 'f' <> O.help "Merge a specific file")
                  )
                  <|> pure Nothing
                )
            )

opts :: O.ParserInfo CmdArgs
opts =
    O.info (O.helper <*> parser) $
    O.fullDesc
    <> O.progDesc
       "Resolve any git conflicts that have become trivial by editing operations.\n\
       \Go to http://github.com/Peaker/git-mediate for example use."
    <> O.header "git-mediate - Become a conflicts hero"

getOpts :: IO Options
getOpts =
    O.execParser opts
    >>= \case
    CmdVersion ->
        do
            putStrLn $ "git-mediate version " ++ versionString
            exitSuccess
    CmdOptions o -> pure o
