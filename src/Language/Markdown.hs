{-# LANGUAGE OverloadedRecordDot #-}

module Language.Markdown (merge) where

import Conflict (Conflict(..), Sides(..))
import Control.Monad (guard)
import Data.List (dropWhileEnd, isPrefixOf)
import qualified Data.List as List
import Data.Maybe (isJust)
import qualified Data.Set as Set

type Chunk = Either String Conflict
type Fence = Maybe (Char, Int)

-- Track fences in all three versions so an earlier conflict cannot turn a
-- code example into a list. Look past the hunk for a detached continuation.
merge :: [Chunk] -> [Either Chunk [String]]
merge = walk (pure Nothing)
  where
    walk :: Sides Fence -> [Chunk] -> [Either Chunk [String]]
    walk _ [] = []
    walk fences (chunk:rest) =
        case chunk of
        Left line -> Left chunk : walk (advance <$> fences <*> pure line) rest
        Right c ->
            let nextFences = List.foldl' advance <$> fences <*> c.bodies
                merged = do
                    guard (all (== Nothing) fences)
                    mergeInsertion rest c.bodies
            in maybe (Left chunk) Right merged : walk nextFences rest

advance :: Fence -> String -> Fence
advance fence line =
    case span (== ' ') line of
    (indent, run@(ch:_)) | length indent <= 3 && ch `elem` "`~" ->
        let (marks, info) = span (== ch) run
            n = length marks
        in case fence of
            Just (open, size) | ch == open && n >= size && blank info -> Nothing
            Nothing | n >= 3 && (ch == '~' || '`' `notElem` info) -> Just (ch, n)
            _ -> fence
    _ -> fence

blank :: String -> Bool
blank = all (`elem` " \t\r")

-- Consume only context present in the ancestor. Shared text between the
-- additions alone may be part of two different multiline items.
mergeInsertion :: [Chunk] -> Sides [String] -> Maybe [String]
mergeInsertion context (Sides ours base theirs) = do
    let prefix = map (\(x,_,_) -> x) $ takeWhile (\(x,y,z) -> x == y && y == z) $
            zip3 base ours theirs
        suffix = drop (length prefix) base
        inserted side = do
            let (middle, end) = splitAt (length side - length base) (drop (length prefix) side)
            guard (end == suffix)
            pure middle
    guard (List.foldl' advance Nothing prefix == Nothing)
    a <- inserted ours
    b <- inserted theirs
    guard (safeEnd False (map Left suffix ++ context))
    (mark, itemsA) <- items a
    (other, itemsB) <- items b
    guard (mark == other && Set.disjoint (Set.fromList itemsA) (Set.fromList itemsB))
    pure (prefix ++ b ++ a ++ suffix)
  where
    -- A following continuation could belong to either side's final item.
    safeEnd _ [] = True
    safeEnd separated (Left line:rest)
        | blank line = safeEnd True rest
        | otherwise = isJust (bullet line) || heading line ||
            (separated && take 1 line `notElem` [" ", "\t"])
    safeEnd _ (Right _:_) = False
    heading line =
        let (hashes, rest) = span (== '#') line
        in not (null hashes) && length hashes <= 6 &&
            (blank rest || take 1 rest `elem` [" ", "\t"])

-- Recognize flush-left unordered lists and their indented continuation
-- blocks. Compare whole entries (ignoring separator blanks) for overlap.
items :: [String] -> Maybe (Char, [[String]])
items ls = do
    first <- case dropWhile blank ls of
        line:_ -> bullet line
        [] -> Nothing
    let parse [] = Just []
        parse (line:rest) = do
            guard (bullet line == Just first)
            let (body, more) = span continuation rest
            ((line : dropWhileEnd blank body) :) <$> parse more
        continuation line = blank line || "  " `isPrefixOf` line || "\t" `isPrefixOf` line
    guard (List.foldl' advance Nothing ls == Nothing)
    entries <- parse (dropWhile blank ls)
    pure (first, entries)

bullet :: String -> Maybe Char
bullet (mark:' ':body)
    | mark `elem` "-*+", not (blank body), not thematic = Just mark
  where
    solid = filter (`notElem` " \t\r") body
    thematic = mark /= '+' && length solid >= 2 && all (== mark) solid
bullet _ = Nothing
