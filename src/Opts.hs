{-# LANGUAGE LambdaCase #-}
-- | Option parser

module Opts
    ( Options(..), EnvOptions(..)
    , getOpts
    ) where

import           Control.Applicative (Alternative(..))
import qualified Options.Applicative as O
import qualified OptUtils
import           PPDiff (ColorEnable(..))
import qualified ResolutionOpts as ResOpts
import           System.Exit (exitSuccess)
import           Version (versionString)

data Options = Options
    { shouldUseEditor :: Bool
    , shouldDumpDiffs :: Bool
    , shouldDumpDiff2 :: Bool
    , shouldUseColor :: Maybe ColorEnable
    , shouldSetConflictStyle :: Bool
    , mergeSpecificFile :: Maybe FilePath
    , envOptions :: EnvOptions
    }

-- Options which can be defaulted from the GIT_MEDIATE_OPTIONS environment variable
data EnvOptions = EnvOptions
    { diffsContext :: Int
    , resolution :: ResOpts.ResolutionOptions
    }

optionsParser :: O.Parser EnvOptions -> O.Parser Options
optionsParser envOpts =
    Options
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
    <*> colorParser
    <*> O.switch
        ( O.long "style" <> O.short 's'
            <> O.help "Configure git's global merge.conflictstyle to diff3 if needed"
        )
    <*> O.optional
        ( O.strOption
            ( O.long "merge-file" <> O.short 'f' <> O.help "Merge a specific file")
        )
    <*> envOpts
    where
        colorParser =
            O.flag' (Just EnableColor)
                (O.long "color" <> O.short 'c' <> O.help "Enable color")
            <|> O.flag' (Just DisableColor)
                (O.long "no-color" <> O.short 'C' <> O.help "Disable color")
            <|> pure Nothing

envOptsParser :: OptUtils.Parser EnvOptions
envOptsParser envOpts =
    EnvOptions
    <$> OptUtils.envOption envOpts "context" (Just 'U')
        ( O.metavar "LINECOUNT" <> O.showDefault <> O.value 3
            <> O.help "Number of context lines around dumped diffs"
        )
    <*> ResOpts.parser envOpts

data CmdArgs = CmdVersion | CmdOptions Options

parser :: O.Parser EnvOptions -> O.Parser CmdArgs
parser envOpts =
    O.flag' CmdVersion (O.long "version" <> O.help "Print the version and quit")
    <|> CmdOptions <$> optionsParser envOpts

opts :: O.Parser EnvOptions -> O.ParserInfo CmdArgs
opts envOpts =
    O.info (O.helper <*> parser envOpts) $
    O.fullDesc
    <> O.progDesc
       "Resolve any git conflicts that have become trivial by editing operations.\n\
       \Go to https://github.com/Peaker/git-mediate for example use."
    <> O.header "git-mediate - Become a conflicts hero"

getOpts :: IO Options
getOpts =
    OptUtils.parseEnvOptions "GIT_MEDIATE_OPTIONS" envOptsParser
    >>= O.execParser . opts
    >>= \case
    CmdVersion ->
        do
            putStrLn $ "git-mediate version " ++ versionString
            exitSuccess
    CmdOptions o -> pure o
