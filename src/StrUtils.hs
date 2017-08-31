{-# LANGUAGE NoImplicitPrelude #-}

module StrUtils
    ( stripNewline, ensureNewline, unprefix
    ) where

import           Data.List (isPrefixOf, isSuffixOf)

import           Prelude.Compat

stripNewline :: String -> String
stripNewline x
    | "\n" `isSuffixOf` x = init x
    | otherwise = x

ensureNewline :: String -> String
ensureNewline "" = ""
ensureNewline str = str ++ suffix
    where
        suffix
            | "\n" `isSuffixOf` str = ""
            | otherwise = "\n"

unprefix :: Eq a => [a] -> [a] -> Maybe [a]
unprefix prefix str
    | prefix `isPrefixOf` str = Just (drop (length prefix) str)
    | otherwise = Nothing
