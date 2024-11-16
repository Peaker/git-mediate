{-# LANGUAGE OverloadedRecordDot #-}

module ResolutionOpts
    ( ResolutionOptions(..), parser, isResolving
    ) where

import           Data.Maybe (isJust)
import qualified OptUtils

data ResolutionOptions = ResolutionOpts
    { trivial :: Bool
    , reduce :: Bool
    , untabify :: Maybe Int
    , lineEndings :: Bool
    , addedLines :: Bool
    , splitMarkers :: Bool
    , indentation :: Bool
    }

parser :: OptUtils.Parser ResolutionOptions
parser =
    ResolutionOpts
    <$> OptUtils.envSwitch "trivial" True "trivial conflicts resolution"
    <*> OptUtils.envSwitch "reduce" True "conflict reduction"
    <*> OptUtils.envOptional "untabify" "TABSIZE"
        "Convert tabs to the spaces at the tab stops for the given tab size"
        (\x -> "Disable converting tabs to " <> show x <> " spaces at the tab stops")
    <*> OptUtils.envSwitch "line-endings" True "line-ending characters conflict resolution"
    <*> OptUtils.envSwitch "lines-added-around" False
        "resolve conflicts where one change prepended lines to the base and the other appended"
    <*> OptUtils.envSwitch "split-markers" True "split conflicts at tilde-split-markers"
    <*> OptUtils.envSwitch "indentation" False "indentation conflict resolution"

isResolving :: ResolutionOptions -> Bool
isResolving o = o.trivial || o.reduce || isJust o.untabify || o.lineEndings || o.addedLines
