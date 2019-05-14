{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, RecordWildCards #-}

module Conflict
    ( Conflict(..), LineNo
    , pretty, prettyLines
    , parse
    , markerPrefix
    ) where

import           Control.Monad.State (MonadState, state, evalStateT)
import           Control.Monad.Writer (runWriter, tell)
import           Data.List (isPrefixOf)

import           Prelude.Compat

type LineNo = Int

data Conflict = Conflict
    { cMarkerA    :: (LineNo, String) -- <<<<<<<....
    , cMarkerBase :: (LineNo, String) -- |||||||....
    , cMarkerB    :: (LineNo, String) -- =======....
    , cMarkerEnd  :: (LineNo, String) -- >>>>>>>....
    , cBodyA      :: [String]
    , cBodyBase   :: [String]
    , cBodyB      :: [String]
    } deriving (Show)

prettyLines :: Conflict -> [String]
prettyLines Conflict {..} =
    concat
    [ snd cMarkerA    : cBodyA
    , snd cMarkerBase : cBodyBase
    , snd cMarkerB    : cBodyB
    , [snd cMarkerEnd]
    ]

pretty :: Conflict -> String
pretty = unlines . prettyLines

-- '>' -> ">>>>>>>"
markerPrefix :: Char -> String
markerPrefix = replicate 7

breakUpToMarker :: MonadState [(LineNo, String)] m => Char -> m [(LineNo, String)]
breakUpToMarker c = state (break ((markerPrefix c `isPrefixOf`) . snd))

readHead :: MonadState [a] m => m (Maybe a)
readHead = state f
    where
        f [] = (Nothing, [])
        f (l:ls) = (Just l, ls)

tryReadUpToMarker :: MonadState [(LineNo, String)] m => Char -> m ([(LineNo, String)], Maybe (LineNo, String))
tryReadUpToMarker c =
    do
        ls <- breakUpToMarker c
        mHead <- readHead
        return (ls, mHead)

readUpToMarker :: MonadState [(LineNo, String)] m => Char -> m ([(LineNo, String)], (LineNo, String))
readUpToMarker c = do
    res <- tryReadUpToMarker c
    case res of
        (ls, Just h)  -> return (ls, h)
        (ls, Nothing) ->
            error $ concat
            [ "Parse error: failed reading up to marker: "
            , show c, ", got:"
            , concatMap (\(l,s) -> "\n" ++ show l ++ "\t" ++ s) $ take 5 ls
            ]

parseConflict :: MonadState [(LineNo, String)] m => (LineNo, String) -> m Conflict
parseConflict markerA =
    do  (linesA   , markerBase) <- readUpToMarker '|'
        (linesBase, markerB)    <- readUpToMarker '='
        (linesB   , markerEnd)  <- readUpToMarker '>'
        return Conflict
            { cMarkerA    = markerA
            , cMarkerBase = markerBase
            , cMarkerB    = markerB
            , cMarkerEnd  = markerEnd
            , cBodyA      = map snd linesA
            , cBodyB      = map snd linesB
            , cBodyBase   = map snd linesBase
            }

parseFromNumberedLines :: [(LineNo, String)] -> [Either String Conflict]
parseFromNumberedLines =
    snd . runWriter . evalStateT loop
    where
        loop =
            do  (ls, mMarkerA) <- tryReadUpToMarker '<'
                tell $ map (Left . snd) ls
                case mMarkerA of
                    Nothing -> return ()
                    Just markerA ->
                        do  tell . return . Right =<< parseConflict markerA
                            loop

parse :: String -> [Either String Conflict]
parse input =
    parseFromNumberedLines (zip [1..] (lines input))
