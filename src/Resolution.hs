{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}

module Resolution
    ( Result(..)
    , NewContent(..)
    , resolveContent
    ) where

import           Conflict (Conflict(..))
import qualified Conflict as Conflict
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
        Conflict.prettyLines conflict
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

data Result = Result
    { _resolvedSuccessfully :: !Int
    , _reducedConflicts :: !Int
    , _failedToResolve :: !Int
    }

instance Semigroup Result where
    Result x0 y0 z0 <> Result x1 y1 z1 = Result (x0+x1) (y0+y1) (z0+z1)

instance Monoid Result where mempty = Result 0 0 0

data NewContent = NewContent
    { _result :: !Result
    , _newContent :: !String
    }

instance Semigroup NewContent where
    NewContent x0 y0 <> NewContent x1 y1 = NewContent (x0<>x1) (y0<>y1)

instance Monoid NewContent where mempty = NewContent mempty mempty

resolveContent :: [Either String Conflict] -> NewContent
resolveContent =
    foldMap go
    where
        go (Left line) = NewContent mempty (unlines [line])
        go (Right conflict) =
            case resolveConflict conflict of
            NoResolution               -> NewContent (Result 0 0 1)
                                          (Conflict.pretty conflict)
            Resolution trivialLines    -> NewContent (Result 1 0 0) trivialLines
            PartialResolution newLines -> NewContent (Result 0 1 0) newLines
