{-# LANGUAGE NoImplicitPrelude, OverloadedRecordDot #-}

module SideDiff
    ( SideDiff(..), Side(..)
    , getConflictDiffs, getConflictDiff2s
    ) where

import           Conflict (Conflict(..), Sides(..), SrcContent(..))
import           Data.Algorithm.Diff (Diff, getDiff)

import           Prelude.Compat

data Side = A | B
    deriving (Eq, Ord, Show)

data SideDiff = SideDiff
    { side :: Side
    , marker :: SrcContent
    , diff :: [Diff String]
    }

getConflictDiffs :: Conflict -> [SideDiff]
getConflictDiffs c =
    [ SideDiff A c.markers.sideA (getDiff c.bodies.sideBase c.bodies.sideA)
    | not (null c.bodies.sideA) ] ++
    [ SideDiff B (SrcContent c.markers.sideB.lineNo c.markerEnd.content) (getDiff c.bodies.sideBase c.bodies.sideB)
    | not (null c.bodies.sideB) ]

getConflictDiff2s :: Conflict -> (SrcContent, SrcContent, [Diff String])
getConflictDiff2s c =
    (c.markers.sideA, c.markers.sideB, getDiff c.bodies.sideA c.bodies.sideB)
