{-# LANGUAGE NoImplicitPrelude, OverloadedRecordDot #-}

module Language.PythonReqs
    ( splitRequirements
    ) where

import           Conflict (Conflict(..), Sides(..))
import           Data.List (isInfixOf, isPrefixOf)
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set

import           Prelude.Compat

splitRequirements :: Conflict -> [Conflict]
splitRequirements conflict =
    case filter (`Set.member` commonModules) modules.sideBase of
    [] -> [conflict]
    (breakModule:_)
        | all null before && all null after -> [conflict]
        | otherwise -> res before <> res (take 1 . snd <$> parts) <> res after
        where
            parts = break ((== Just breakModule) . pyReqLineModule) <$> conflict.bodies
            before = fst <$> parts
            after = drop 1 . snd <$> parts
            res x = splitRequirements conflict{Conflict.bodies = x}
    where
        modules = mapMaybe pyReqLineModule <$> conflict.bodies
        commonModules = foldl1 Set.intersection (Set.fromList <$> modules)

pyReqLineModule :: String -> Maybe String
pyReqLineModule line
    | "==" `isInfixOf` line = Just (takeWhile (/= '=') line)
    | gitHttpsPrefix `isPrefixOf` line =
        Just $ dropWhile (/= '.') $ reverse $ dropWhile (/= '/') $ reverse line
    | otherwise = Nothing
    where
        gitHttpsPrefix = "git+https://"
