{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}

module Resolution
    ( Resolution(..)
    , resolveConflict
    ) where

import           Conflict (Conflict(..), prettyConflictLines)

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
