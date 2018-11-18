{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}

module Resolution
    ( NewContent(..)
    , resolveContent
    ) where

import           Conflict (Conflict(..), prettyConflictLines, prettyConflict)
import qualified Data.Monoid as Monoid

import           Prelude.Compat

data Resolution
    = NoResolution
    | Resolution String
    | PartialResolution String

resolveConflict :: Conflict -> Resolution
resolveConflict conflict@Conflict{..}
    | cBodyA == cBodyBase = Resolution $ unlines cBodyB
    | cBodyB == cBodyBase = Resolution $ unlines cBodyA
    | cBodyA == cBodyB = Resolution $ unlines cBodyA
    | matchTop > 0 || matchBottom > 0 =
        PartialResolution $ unlines $
        take matchTop cBodyBase ++
        prettyConflictLines conflict
        { cBodyA = unmatched cBodyA
        , cBodyBase = unmatched cBodyBase
        , cBodyB = unmatched cBodyB
        } ++
        takeEnd matchBottom cBodyBase
    | otherwise = NoResolution
    where
        matchTop =
            minimum $ map (lengthOfCommonPrefix cBodyBase) [cBodyA, cBodyB]
        revBottom = reverse . drop matchTop
        revBottomBase = revBottom cBodyBase
        matchBottom =
            minimum $
            map (lengthOfCommonPrefix revBottomBase . revBottom)
            [cBodyA, cBodyB]
        dropEnd count xs = take (length xs - count) xs
        takeEnd count xs = drop (length xs - count) xs
        unmatched xs = drop matchTop $ dropEnd matchBottom xs

lengthOfCommonPrefix :: Eq a => [a] -> [a] -> Int
lengthOfCommonPrefix x y = length $ takeWhile id $ zipWith (==) x y

data NewContent = NewContent
    { _resolvedSuccessfully :: Int
    , _reducedConflicts :: Int
    , _failedToResolve :: Int
    , _newContent :: String
    }

resolveContent :: [Either String Conflict] -> NewContent
resolveContent =
    asResult . mconcat . map go
    where
        asResult (Monoid.Sum successes, Monoid.Sum reductions, Monoid.Sum failures, newContent) =
            NewContent
            { _resolvedSuccessfully = successes
            , _reducedConflicts = reductions
            , _failedToResolve = failures
            , _newContent = newContent
            }
        go (Left line) = (Monoid.Sum 0, Monoid.Sum 0, Monoid.Sum 0, unlines [line])
        go (Right conflict) =
            case resolveConflict conflict of
            NoResolution -> (Monoid.Sum 0, Monoid.Sum 0, Monoid.Sum 1, prettyConflict conflict)
            Resolution trivialLines -> (Monoid.Sum 1, Monoid.Sum 0, Monoid.Sum 0, trivialLines)
            PartialResolution newLines -> (Monoid.Sum 0, Monoid.Sum 1, Monoid.Sum 0, newLines)
