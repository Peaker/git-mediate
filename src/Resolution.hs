{-# LANGUAGE NoImplicitPrelude, BangPatterns, NamedFieldPuns #-}

module Resolution
    ( Result(..)
    , NewContent(..)
    , Untabify(..)
    , resolveContent
    , fullySuccessful
    ) where

import           Conflict (Conflict(..), Sides(..))
import qualified Conflict

import           Prelude.Compat

data Resolution
    = NoResolution
    | Resolution String
    | PartialResolution String

resolveGen :: Eq a => Sides a -> Maybe a
resolveGen (Sides a base b)
    | a == base = Just b
    | b == base = Just a
    | a == b = Just a
    | otherwise = Nothing

resolveConflict :: Conflict -> Resolution
resolveConflict conflict@Conflict {cBodies} =
    case resolveGen cBodies of
    Just r -> Resolution $ unlines r
    Nothing
        | matchTop > 0 || matchBottom > 0 ->
            PartialResolution $ unlines $
            take matchTop sideA ++
            Conflict.prettyLines ((Conflict.setBodies . fmap) unmatched conflict) ++
            takeEnd matchBottom sideA
        | otherwise -> NoResolution
    where
        Sides {sideA, sideBase, sideB} = cBodies
        match base a b
            | null base = lengthOfCommonPrefix a b
            | otherwise = minimum $ map (lengthOfCommonPrefix base) [a, b]
        matchTop = match sideBase sideA sideB
        revBottom = reverse . drop matchTop
        matchBottom = match (revBottom sideBase) (revBottom sideA) (revBottom sideB)
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

fullySuccessful :: Result -> Bool
fullySuccessful (Result _ reduced failed) = reduced == 0 && failed == 0

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

newtype Untabify = Untabify { mUntabifySize :: Maybe Int }

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

untabify :: Int -> Conflict -> Conflict
untabify = Conflict.setBodies . fmap . fmap . untabifyStr

resolveContent :: Untabify -> [Either String Conflict] -> NewContent
resolveContent (Untabify mUntabifySize) =
    foldMap go
    where
        untabified = maybe id untabify mUntabifySize
        go (Left line) = NewContent mempty (unlines [line])
        go (Right conflict) =
            case resolveConflict (untabified conflict) of
            NoResolution               -> NewContent (Result 0 0 1)
                                          (Conflict.pretty (untabified conflict))
            Resolution trivialLines    -> NewContent (Result 1 0 0) trivialLines
            PartialResolution newLines -> NewContent (Result 0 1 0) newLines
