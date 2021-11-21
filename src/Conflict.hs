{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, DeriveTraversable, NamedFieldPuns, DerivingVia, DeriveGeneric #-}

module Conflict
    ( Conflict(..), Sides(..), LineNo
    , setEachBody, setStrings
    , pretty, prettyLines
    , parse
    ) where

import Control.Monad.State (MonadState, state, evalStateT)
import Control.Monad.Writer (runWriter, tell)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Generic.Data (Generically1(..))
import GHC.Generics (Generic1)

import Prelude.Compat

type LineNo = Int

data Sides a = Sides
    { sideA :: a
    , sideBase :: a
    , sideB :: a
    } deriving (Functor, Foldable, Traversable, Show, Eq, Ord, Generic1)
    deriving Applicative via Generically1 Sides

data Conflict = Conflict
    { cMarkers   :: Sides (LineNo, String) -- The markers at the beginning of sections
    , cMarkerEnd :: (LineNo, String)       -- The ">>>>>>>...." marker at the end of the conflict
    , cBodies    :: Sides [String]
    } deriving (Show)

-- traversal
bodies :: Applicative f => (Sides [String] -> f (Sides [String])) -> Conflict -> f Conflict
bodies f c@Conflict{cBodies} = (\x -> c{cBodies = x}) <$> f cBodies

-- setter:
setBodies :: (Sides [String] -> Sides [String]) -> Conflict -> Conflict
setBodies f = runIdentity . bodies (Identity . f)

setEachBody :: ([String] -> [String]) -> Conflict -> Conflict
setEachBody = setBodies . fmap

setStrings :: (String -> String) -> Conflict -> Conflict
setStrings = setEachBody . map

prettyLines :: Conflict -> [String]
prettyLines Conflict{cMarkers, cMarkerEnd, cBodies} =
    concat ((:) <$> (snd <$> cMarkers) <*> cBodies) <> [snd cMarkerEnd]

pretty :: Conflict -> String
pretty = unlines . prettyLines

breakUpToMarker ::
    MonadState [(LineNo, String)] m =>
    Char -> Maybe Int -> m [(LineNo, String)]
breakUpToMarker c mCount =
    state (break cond)
    where
        count = fromMaybe 7 mCount
        prefix = replicate count c
        cond (_, line) =
            pre == prefix && rightCount
            where
                (pre, post) = splitAt count line
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
    MonadState [(LineNo, String)] m =>
    Char -> Maybe Int -> m ([(LineNo, String)], Maybe (LineNo, String))
tryReadUpToMarker c mCount =
    (,) <$> breakUpToMarker c mCount <*> readHead

readUpToMarker ::
    MonadState [(LineNo, String)] m =>
    Char -> Maybe Int -> m ([(LineNo, String)], (LineNo, String))
readUpToMarker c mCount = do
    res <- tryReadUpToMarker c mCount
    case res of
        (ls, Just h)  -> pure (ls, h)
        (ls, Nothing) ->
            error $ concat
            [ "Parse error: failed reading up to marker: "
            , show c, ", got:"
            , concatMap (\(l,s) -> "\n" ++ show l ++ "\t" ++ s) $ take 5 ls
            ]

parseConflict :: MonadState [(LineNo, String)] m => (LineNo, String) -> m Conflict
parseConflict markerA =
    do  (linesA   , markerBase) <- readUpToMarker '|' markerCount
        (linesBase, markerB)    <- readUpToMarker '=' markerCount
        (linesB   , markerEnd)  <- readUpToMarker '>' markerCount
        pure Conflict
            { cMarkers    = Sides markerA markerBase markerB
            , cMarkerEnd  = markerEnd
            , cBodies     = fmap snd <$> Sides linesA linesBase linesB
            }
    where
        markerCount = Just (length (takeWhile (== '<') (snd markerA)))

parseFromNumberedLines :: [(LineNo, String)] -> [Either String Conflict]
parseFromNumberedLines =
    snd . runWriter . evalStateT loop
    where
        loop =
            do  (ls, mMarkerA) <- tryReadUpToMarker '<' Nothing
                tell $ map (Left . snd) ls
                case mMarkerA of
                    Nothing -> pure ()
                    Just markerA ->
                        do  tell . pure . Right =<< parseConflict markerA
                            loop

parse :: String -> [Either String Conflict]
parse = parseFromNumberedLines . zip [1..] . lines
