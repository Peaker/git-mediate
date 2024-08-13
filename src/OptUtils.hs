{-# LANGUAGE OverloadedRecordDot, LambdaCase #-}

module OptUtils
    ( EnvOpts, readEnv, envSwitch, envOption
    ) where

import           Control.Monad (unless)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Options.Applicative as O
import           System.Environment (lookupEnv)
import           Text.Read.Compat (readMaybe)

data EnvOpts = EnvOpts
    { envVarName :: String
    , flags :: S.Set String
    , options :: M.Map String String
    }

readEnv :: String -> IO EnvOpts
readEnv name =
    lookupEnv name >>=
    \case
    Nothing -> pure (EnvOpts name mempty mempty)
    Just opts ->
        EnvOpts name f o <$
        unless (null r) (putStrLn ("Warning: unrecognized options in " <> name <> ": " <> unwords r <> "\n"))
        where
            (r, (f, o)) = parseEnv (words opts)

parseEnv :: [String] -> ([String], (S.Set String, M.Map String String))
parseEnv [] = mempty
parseEnv (('-':'-':flag):rest) =
    case rest of
    [] -> parseFlag
    ('-':'-':_):_ -> parseFlag
    val:rest' -> (mempty, (mempty, M.singleton flag val)) <> parseEnv rest'
    where
        parseFlag = (mempty, (S.singleton flag, mempty)) <> parseEnv rest
parseEnv (other:rest) = ([other], mempty) <> parseEnv rest

envSwitch :: EnvOpts -> String -> Bool -> String -> O.Parser Bool
envSwitch envOpts name def desc =
    (/= (def /= otherInEnv)) <$> O.switch (O.long flag <> O.help help)
    where
        flag = if otherInEnv then defaultMode else otherMode
        help =
            (if curDef then "Disable " else "Enable ")
            <> desc
            <> if otherInEnv then overrideHelp envOpts otherMode else ""
        curDef = def /= otherInEnv
        noFlag = "no-" <> name
        (defaultMode, otherMode) = if def then (name, noFlag) else (noFlag, name)
        otherInEnv = S.member otherMode envOpts.flags

overrideHelp :: EnvOpts -> String -> String
overrideHelp envOpts val = " (override \"--" <> val <> "\" from " <> envOpts.envVarName <> ")"

envOption :: (Read a, Show a) => EnvOpts -> String -> String -> String -> (a -> String) -> O.Parser (Maybe a)
envOption envOpts name valDesc help disableHelp =
    case M.lookup name envOpts.options >>= readMaybe of
    Just val ->
        def oh O.<|> (f <$> O.switch (O.long ("no-" <> name) <> O.help h))
        where
            oh = overrideHelp envOpts (name <> " " <> show val)
            h = disableHelp val <> oh
            f True = Nothing
            f False = Just val
    Nothing -> def ""
    where
        def suffix = Just <$> O.option O.auto (O.long name <> O.metavar valDesc <> O.help (help <> suffix))