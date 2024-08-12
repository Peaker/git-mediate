{-# LANGUAGE OverloadedRecordDot #-}

module ResolutionOpts
    ( ResolutionOptions(..), parser, isResolving
    ) where

import           Data.Maybe (isJust)
import qualified Options.Applicative as O
import qualified OptUtils

data ResolutionOptions = ResolutionOpts
    { trivial :: Bool
    , reduce :: Bool
    , untabify :: Maybe Int
    , lineEndings :: Bool
    , addedLines :: Bool
    }

parser :: OptUtils.EnvOpts -> O.Parser ResolutionOptions
parser envOpts =
    ResolutionOpts
    <$> OptUtils.envSwitch envOpts "trivial" True "trivial conflicts resolution"
    <*> OptUtils.envSwitch envOpts "reduce" True "conflict reduction"
    <*> O.optional
        ( O.option O.auto
            ( O.long "untabify" <> O.metavar "TABSIZE"
                <> O.help "Convert tabs to the spaces at the tab stops for the given tab size"
            )
        )
    <*> OptUtils.envSwitch envOpts "line-endings" True "line-ending characters conflict resolution"
    <*> OptUtils.envSwitch envOpts "added-lines" False "added lines resolution (EXPERIMENTAL)"

isResolving :: ResolutionOptions -> Bool
isResolving o = o.trivial || o.reduce || isJust o.untabify || o.lineEndings || o.addedLines
