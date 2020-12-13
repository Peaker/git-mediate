{-# LANGUAGE NoImplicitPrelude, NamedFieldPuns #-}

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
getConflictDiffs Conflict{cMarkers, cMarkerEnd, cBodies} =
    [ (A, markerA, getDiff bodyBase bodyA)
    | not (null bodyA) ] ++
    [ (B, (fst markerB, snd cMarkerEnd), getDiff bodyBase bodyB)
    | not (null bodyB) ]
    where
        Sides markerA _ markerB = cMarkers
        Sides bodyA bodyBase bodyB = cBodies

getConflictDiff2s :: Conflict -> ((LineNo, String), (LineNo, String), [Diff String])
getConflictDiff2s Conflict{cMarkers, cBodies} =
    (markerA, markerB, getDiff bodyA bodyB)
    where
        Sides markerA _ markerB = cMarkers
        Sides bodyA _ bodyB = cBodies
