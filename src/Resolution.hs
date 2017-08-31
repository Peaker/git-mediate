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
    | cLinesA == cLinesBase = Resolution $ unlines cLinesB
    | cLinesB == cLinesBase = Resolution $ unlines cLinesA
    | cLinesA == cLinesB = Resolution $ unlines cLinesA
    | matchTop > 0 || matchBottom > 0 =
        PartialResolution $ unlines $
        take matchTop cLinesBase ++
        prettyConflictLines conflict
        { cLinesA = unmatched cLinesA
        , cLinesBase = unmatched cLinesBase
        , cLinesB = unmatched cLinesB
        } ++
        takeEnd matchBottom cLinesBase
    | otherwise = NoResolution
    where
        matchTop =
            minimum $ map (lengthOfCommonPrefix cLinesBase) [cLinesA, cLinesB]
        revBottom = reverse . drop matchTop
        revBottomBase = revBottom cLinesBase
        matchBottom =
            minimum $
            map (lengthOfCommonPrefix revBottomBase . revBottom)
            [cLinesA, cLinesB]
        dropEnd count xs = take (length xs - count) xs
        takeEnd count xs = drop (length xs - count) xs
        unmatched xs = drop matchTop $ dropEnd matchBottom xs

lengthOfCommonPrefix :: Eq a => [a] -> [a] -> Int
lengthOfCommonPrefix x y = length $ takeWhile id $ zipWith (==) x y
