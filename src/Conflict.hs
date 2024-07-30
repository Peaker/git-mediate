{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, DeriveTraversable, NamedFieldPuns #-}
{-# LANGUAGE DerivingVia, DeriveGeneric, OverloadedRecordDot, LambdaCase #-}

module Conflict
    ( Conflict(..), Sides(..), SrcContent(..)
    , setEachBody, setStrings
    , pretty, prettyLines
    , parse
    ) where

import Control.Monad.State (MonadState, evalStateT, state)
import Control.Monad.Writer (runWriter, tell)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic1)
import Generic.Data (Generically1(..))

import Prelude.Compat

data Sides a = Sides
    { sideA :: a
    , sideBase :: a
    , sideB :: a
    }
    deriving (Functor, Foldable, Traversable, Show, Eq, Ord, Generic1)
    deriving (Applicative) via Generically1 Sides

data SrcContent = SrcContent
    { lineNo :: Int
    , content :: String
    }
    deriving (Show)

data Conflict = Conflict
    { markers :: Sides SrcContent -- The markers at the beginning of sections
    , markerEnd :: SrcContent -- The ">>>>>>>...." marker at the end of the conflict
    , bodies :: Sides [String]
    }
    deriving (Show)

setBodies :: (Sides [String] -> Sides [String]) -> Conflict -> Conflict
setBodies f c = c{bodies = f c.bodies}

setEachBody :: ([String] -> [String]) -> Conflict -> Conflict
setEachBody = setBodies . fmap

setStrings :: (String -> String) -> Conflict -> Conflict
setStrings = setEachBody . map

prettyLines :: Conflict -> [String]
prettyLines c =
    concat ((:) . content <$> c.markers <*> c.bodies) <> [c.markerEnd.content]

pretty :: Conflict -> String
pretty = unlines . prettyLines

breakUpToMarker ::
    MonadState [SrcContent] m =>
    Char -> Maybe Int -> m [SrcContent]
breakUpToMarker c mCount =
    state (break cond)
    where
        count = fromMaybe 7 mCount
        prefix = replicate count c
        cond l =
            pre == prefix && rightCount
            where
                (pre, post) = splitAt count l.content
                rightCount =
                    case (mCount, post) of
                    (Just{}, x:_) -> c /= x
                    _ -> True

readHead :: MonadState [a] m => m (Maybe a)
readHead = state f
    where
        f [] = (Nothing, [])
        f (l:ls) = (Just l, ls)

tryReadUpToMarker ::
    MonadState [SrcContent] m =>
    Char -> Maybe Int -> m ([SrcContent], Maybe SrcContent)
tryReadUpToMarker c mCount =
    (,) <$> breakUpToMarker c mCount <*> readHead

readUpToMarker ::
    MonadState [SrcContent] m =>
    Char -> Maybe Int -> m ([SrcContent], SrcContent)
readUpToMarker c mCount =
    tryReadUpToMarker c mCount >>=
    \case
    (ls, Just h) -> pure (ls, h)
    (ls, Nothing) ->
        error $ concat
        [ "Parse error: failed reading up to marker: "
        , show c, ", got:"
        , concatMap (\l -> "\n" ++ show l.lineNo ++ "\t" ++ l.content) $ take 5 ls
        ]

parseConflict :: MonadState [SrcContent] m => SrcContent -> m Conflict
parseConflict markerA =
    do
        (linesA, markerBase) <- readUpToMarker '|' markerCount
        (linesBase, markerB)    <- readUpToMarker '=' markerCount
        (linesB   , markerEnd)  <- readUpToMarker '>' markerCount
        pure Conflict
            { markers    = Sides markerA markerBase markerB
            , markerEnd
            , bodies     = fmap (.content) <$> Sides linesA linesBase linesB
            }
    where
        markerCount = Just (length (takeWhile (== '<') markerA.content))

parseFromNumberedLines :: [SrcContent] -> [Either String Conflict]
parseFromNumberedLines =
    snd . runWriter . evalStateT loop
    where
        loop =
            do
                (ls, mMarkerA) <- tryReadUpToMarker '<' Nothing
                tell $ map (Left . (.content)) ls
                case mMarkerA of
                    Nothing -> pure ()
                    Just markerA ->
                        do
                            tell . pure . Right =<< parseConflict markerA
                            loop

parse :: String -> [Either String Conflict]
parse = parseFromNumberedLines . zipWith SrcContent [1 ..] . lines
