{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}

module SideDiff
    ( SideDiff, Side(..)
    , getConflictDiffs, getConflictDiff2s
    ) where

import           Conflict (Conflict(..), LineNo)
import           Data.Algorithm.Diff (Diff, getDiff)

import           Prelude.Compat

data Side = A | B
    deriving (Eq, Ord, Show)

type SideDiff = (Side, (LineNo, String), [Diff String])

getConflictDiffs :: Conflict -> [SideDiff]
getConflictDiffs Conflict{..} =
    [ (A, cMarkerA, getDiff cLinesBase cLinesA)
    | not (null cLinesA) ] ++
    [ (B, (fst cMarkerB, snd cMarkerEnd), getDiff cLinesBase cLinesB)
    | not (null cLinesB) ]

getConflictDiff2s :: Conflict -> ((LineNo, String), (LineNo, String), [Diff String])
getConflictDiff2s Conflict{..} = (cMarkerA, cMarkerB, getDiff cLinesA cLinesB)
