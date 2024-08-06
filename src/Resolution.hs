{-# LANGUAGE NoImplicitPrelude, BangPatterns, DerivingVia, DeriveGeneric, OverloadedRecordDot #-}

module Resolution
    ( Result(..)
    , NewContent(..)
    , resolveContent
    , fullySuccessful
    ) where

import           Conflict (Conflict(..), Sides(..))
import qualified Conflict
import           Data.Foldable (Foldable(..))
import           Generic.Data (Generic, Generically(..))
import           ResolutionOpts

import           Prelude.Compat

data Resolution
    = NoResolution Conflict
    | Resolution String
    | PartialResolution String

resolveGen :: Eq a => Sides a -> Maybe a
resolveGen (Sides a base b)
    | a == base = Just b
    | b == base = Just a
    | a == b = Just a
    | otherwise = Nothing

reduceConflict :: Conflict -> Maybe String
reduceConflict c
    | matchTop > 0 || matchBottom > 0 =
        Just $ unlines $
        take matchTop c.bodies.sideA ++
        Conflict.prettyLines (Conflict.setEachBody unmatched c) ++
        takeEnd matchBottom c.bodies.sideA
    | otherwise = Nothing
    where
        match base a b
            | null base = lengthOfCommonPrefix a b
            | otherwise = minimum $ map (lengthOfCommonPrefix base) [a, b]
        matchTop = match c.bodies.sideBase c.bodies.sideA c.bodies.sideB
        revBottom = reverse . drop matchTop
        matchBottom = match (revBottom c.bodies.sideBase) (revBottom c.bodies.sideA) (revBottom c.bodies.sideB)
        dropEnd count xs = take (length xs - count) xs
        takeEnd count xs = drop (length xs - count) xs
        unmatched xs = drop matchTop $ dropEnd matchBottom xs

-- | Like 'maybe', but with the arguments flipped.
maybe' :: (a -> b) -> Maybe a -> b -> b
maybe' f x def = maybe def f x

resolveConflict :: Conflict -> Resolution
resolveConflict c =
    maybe' (Resolution . unlines) (resolveGen c.bodies) $
    maybe' PartialResolution (reduceConflict c) $
    NoResolution c

lengthOfCommonPrefix :: Eq a => [a] -> [a] -> Int
lengthOfCommonPrefix x y = length $ takeWhile id $ zipWith (==) x y

data Result = Result
    { resolvedSuccessfully :: !Int
    , reducedConflicts :: !Int
    , failedToResolve :: !Int
    }

fullySuccessful :: Result -> Bool
fullySuccessful (Result _ reduced failed) = reduced == 0 && failed == 0

instance Semigroup Result where
    Result x0 y0 z0 <> Result x1 y1 z1 = Result (x0+x1) (y0+y1) (z0+z1)

instance Monoid Result where mempty = Result 0 0 0

data NewContent = NewContent
    { result :: !Result
    , newContent :: !String
    }
    deriving Generic
    deriving (Semigroup, Monoid) via Generically NewContent

untabifyStr :: Int -> String -> String
untabifyStr size =
    go 0
    where
        cyclicInc col
            | col >= size - 1 = 0
            | otherwise = col + 1
        go !col ('\t':rest) = replicate (size - col) ' ' ++ go 0 rest
        go !col (x:rest) = x : go (cyclicInc col) rest
        go _ [] = []

untabifyConflict :: Int -> Conflict -> Conflict
untabifyConflict = Conflict.setStrings . untabifyStr

data LineEnding = LF | CRLF | Mixed
    deriving (Eq, Ord)

lineEnding :: String -> LineEnding
lineEnding x@(_:_) | last x == '\r' = CRLF
lineEnding _ = LF

lineEndings :: [String] -> LineEnding
lineEndings [] = Mixed
lineEndings xs =
    foldl1 f (map lineEnding xs)
    where
        f Mixed _ = Mixed
        f x c
            | x == c = x
            | otherwise = Mixed

allSame :: Eq a => [a] -> Bool
allSame (x:y:rest) = x == y && allSame (y:rest)
allSame _ = True

lineBreakFix :: Conflict -> Conflict
lineBreakFix c
    | any null (toList c.bodies)
    || allSame (toList endings) = c
    | otherwise =
        case resolveGen endings of
        Just LF -> Conflict.setStrings removeCr c
        Just CRLF -> Conflict.setStrings makeCr c
        _ -> c
    where
        endings = fmap lineEndings c.bodies
        removeCr x@(_:_) | last x == '\r' = init x
        removeCr x = x
        makeCr x@(_:_) | last x == '\r' = x
        makeCr x = x <> "\r"

formatResolution :: Resolution -> NewContent
formatResolution (NoResolution c) = NewContent (Result 0 0 1) (Conflict.pretty c)
formatResolution (Resolution trivialLines) = NewContent (Result 1 0 0) trivialLines
formatResolution (PartialResolution newLines) = NewContent (Result 0 1 0) newLines

resolveContent :: ResolutionOptions -> [Either String Conflict] -> NewContent
resolveContent opts =
    foldMap go
    where
        untabified = maybe id untabifyConflict opts.untabify
        go (Left line) = NewContent mempty (unlines [line])
        go (Right conflict) = formatResolution $ resolveConflict $ lineBreakFix $ untabified conflict
