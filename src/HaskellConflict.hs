{-# LANGUAGE NoImplicitPrelude, OverloadedRecordDot #-}

module HaskellConflict (splitImports) where

import Conflict (Conflict(..), Sides(..))

import Prelude.Compat

-- Split aligned imports at the edges of a conflict into independent conflicts.
-- This lets the ordinary trivial resolver combine changes to different imports.
splitImports :: Conflict -> [Conflict]
splitImports c
    | all null c.bodies = []
    | Just (row, rest) <- takeImportRow c.bodies =
        c{bodies = pure <$> row} : splitImports c{bodies = rest}
    | Just (rest, row) <- takeEndImportRow c.bodies =
        splitImports c{bodies = rest} <> [c{bodies = pure <$> row}]
    | otherwise = [c]

takeImportRow :: Sides [String] -> Maybe (Sides String, Sides [String])
takeImportRow (Sides (a:as) (base:bases) (b:bs)) =
    let row = Sides a base b
    in if sameImportModule row then Just (row, Sides as bases bs) else Nothing
takeImportRow _ = Nothing

takeEndImportRow :: Sides [String] -> Maybe (Sides [String], Sides String)
takeEndImportRow = fmap swap . takeImportRow . fmap reverse
    where
        swap (row, rest) = (fmap reverse rest, row)

sameImportModule :: Sides String -> Bool
sameImportModule sides =
    case traverse importModule sides of
        Just (Sides a base b) -> a == base && base == b
        Nothing -> False

importModule :: String -> Maybe String
importModule line =
    case words line of
        "import":rest -> firstModule rest
        _ -> Nothing
    where
        firstModule [] = Nothing
        firstModule (word@(first:_):rest)
            | word `elem` ["qualified", "safe"] = firstModule rest
            | first == '"' = firstModule rest
            | first >= 'A' && first <= 'Z' = Just word
            | otherwise = Nothing
        firstModule ([]:rest) = firstModule rest
