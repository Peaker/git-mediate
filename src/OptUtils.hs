{-# LANGUAGE OverloadedRecordDot, LambdaCase #-}

module OptUtils
    ( EnvOpts, readEnv, envSwitch, envOptional, envOption
    ) where

import           Control.Applicative ((<|>))
import           Control.Monad (unless)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Options.Applicative as O
import           System.Environment (lookupEnv)
import           Text.Read.Compat (readMaybe)

data EnvOpts = EnvOpts
    { envVarName :: String
    , content :: EnvContent
    }

data EnvContent = EnvContent
    { flags :: S.Set String
    , options :: M.Map String String
    , remainder :: [String]
    }

instance Semigroup EnvContent where
    EnvContent f0 o0 r0 <> EnvContent f1 o1 r1 = EnvContent (f0 <> f1) (o0 <> o1) (r0 <> r1)

instance Monoid EnvContent where
    mempty = EnvContent mempty mempty mempty

readEnv :: String -> IO EnvOpts
readEnv name =
    lookupEnv name >>=
    \case
    Nothing -> pure (EnvOpts name mempty)
    Just opts ->
        EnvOpts name c <$
        unless (null c.remainder)
        (putStrLn ("Warning: unrecognized options in " <> name <> ": " <> unwords c.remainder <> "\n"))
        where
            c = parseEnv (words opts)

parseEnv :: [String] -> EnvContent
parseEnv [] = mempty
parseEnv (('-':'-':flag):rest) =
    case rest of
    [] -> parseFlag
    ('-':'-':_):_ -> parseFlag
    val:rest' -> mempty{options = M.singleton flag val} <> parseEnv rest'
    where
        parseFlag = mempty{flags = S.singleton flag} <> parseEnv rest
parseEnv (other:rest) = mempty{remainder = [other]} <> parseEnv rest

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
        otherInEnv = S.member otherMode envOpts.content.flags

overrideHelp :: EnvOpts -> String -> String
overrideHelp envOpts val = " (override \"--" <> val <> "\" from " <> envOpts.envVarName <> ")"

envOptional :: (Read a, Show a) => EnvOpts -> String -> String -> String -> (a -> String) -> O.Parser (Maybe a)
envOptional envOpts name valDesc help disableHelp =
    case M.lookup name envOpts.content.options >>= readMaybe of
    Just val ->
        def oh <|> f <$> O.switch (O.long ("no-" <> name) <> O.help h)
        where
            oh = overrideHelp envOpts (name <> " " <> show val)
            h = disableHelp val <> oh
            f True = Nothing
            f False = Just val
    Nothing -> def ""
    where
        def suffix = O.optional (O.option O.auto (O.long name <> O.metavar valDesc <> O.help (help <> suffix)))

envOption :: (Read a, Show a) => EnvOpts -> String -> Maybe Char -> O.Mod O.OptionFields a -> O.Parser a
envOption envOpts name shortName mods =
    O.option O.auto
    (O.long name <> foldMap O.short shortName <> mods <> opts)
    where
        opts = foldMap (envOptionFromEnv envOpts) (l name <|> (shortName >>= l . pure))
        l n = M.lookup n envOpts.content.options >>= readMaybe

envOptionFromEnv :: Show a => EnvOpts -> a -> O.Mod O.OptionFields a
envOptionFromEnv envOpts val =
    O.value val <> O.showDefaultWith (\x -> show x <> ", from " <> envOpts.envVarName)
