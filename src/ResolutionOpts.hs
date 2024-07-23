{-# LANGUAGE OverloadedRecordDot #-}

module ResolutionOpts
    ( ResolutionOptions(..), parser, isResolving
    ) where

import           Data.Maybe (isJust)
import qualified Options.Applicative as O

data ResolutionOptions = ResolutionOpts
    { trivial :: Bool
    , reduce :: Bool
    , untabify :: Maybe Int
    , lineEndings :: Bool
    , addedLines :: Bool
    }

parser :: O.Parser ResolutionOptions
parser =
    ResolutionOpts
    <$> noSwitch "trivial" "Disable trivial conflicts resolution"
    <*> noSwitch "reduce" "Disable conflict reduction"
    <*> O.optional
        ( O.option O.auto
            ( O.long "untabify" <> O.metavar "TABSIZE"
                <> O.help "Convert tabs to the spaces at the tab stops for the given tab size"
            )
        )
    <*> noSwitch "line-endings" "Do not fix line-ending characters conflicts"
    <*> O.switch (O.long "added-lines" <> O.help "EXPERIMENTAL: Resolve added lines")
    where
        noSwitch s t = not <$> O.switch (O.long ("no-" <> s) <> O.help t)

isResolving :: ResolutionOptions -> Bool
isResolving o = o.trivial || o.reduce || isJust o.untabify || o.lineEndings || o.addedLines
