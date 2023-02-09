{-# LANGUAGE NoImplicitPrelude, OverloadedRecordDot #-}

module SideDiff
    ( SideDiff, Side(..)
    , getConflictDiffs, getConflictDiff2s
    ) where

import           Conflict (Conflict(..), Sides(..), LineNo)
import           Data.Algorithm.Diff (Diff, getDiff)

import           Prelude.Compat

data Side = A | B
    deriving (Eq, Ord, Show)

type SideDiff = (Side, (LineNo, String), [Diff String])

getConflictDiffs :: Conflict -> [SideDiff]
getConflictDiffs c =
    [ (A, c.markers.sideA, getDiff c.bodies.sideBase c.bodies.sideA)
    | not (null c.bodies.sideA) ] ++
    [ (B, (fst c.markers.sideB, snd c.markerEnd), getDiff c.bodies.sideBase c.bodies.sideB)
    | not (null c.bodies.sideB) ]

getConflictDiff2s :: Conflict -> ((LineNo, String), (LineNo, String), [Diff String])
getConflictDiff2s c =
    (c.markers.sideA, c.markers.sideB, getDiff c.bodies.sideA c.bodies.sideB)
