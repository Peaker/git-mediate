{-# LANGUAGE NoImplicitPrelude #-}

module StrUtils
    ( (</>), stripNewline, ensureNewline
    ) where

import           Data.List (isSuffixOf)
import qualified System.FilePath as FilePath

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

(</>) :: FilePath -> FilePath -> FilePath
"." </> p = p
d </> p = d FilePath.</> p
