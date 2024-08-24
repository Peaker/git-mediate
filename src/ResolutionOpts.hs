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
    <*> OptUtils.envSwitch "added-lines" False "added lines resolution (EXPERIMENTAL)"

isResolving :: ResolutionOptions -> Bool
isResolving o = o.trivial || o.reduce || isJust o.untabify || o.lineEndings || o.addedLines
