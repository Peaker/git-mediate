{-# LANGUAGE OverloadedRecordDot #-}

module OptUtils
    ( EnvOpts, parseEnvOptions, envSwitch, envOptional, envOption
    ) where

import           Control.Applicative ((<|>))
import           Control.Monad (guard, unless)
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
    , errors :: [String]
    }

instance Semigroup EnvContent where
    EnvContent f0 o0 e0 <> EnvContent f1 o1 e1 =
        EnvContent (f0 <> f1) (o0 <> o1)
        ( e0 <> e1
            <> (("Duplicate flag: " <>) <$> S.toList (S.intersection f0 f1))
            <> (("Duplicate option: " <>) <$> M.keys (M.intersection o0 o1))
        )

instance Monoid EnvContent where
    mempty = EnvContent mempty mempty mempty

parseEnvOptions :: String -> (EnvOpts -> O.Parser a) -> IO (O.Parser a)
parseEnvOptions name parser =
    do
        envOpts <- foldMap (parseEnv . words) <$> lookupEnv name
        unless (null envOpts.errors)
            (putStrLn (unlines (
                ("Warning: unrecognized options in " <> name <> ":")
                    : (("  * " <>) <$> envOpts.errors))))
        pure (parser (EnvOpts name envOpts))

parseEnv :: [String] -> EnvContent
parseEnv [] = mempty
parseEnv (('-':'-':flag@(_:_)):rest) = parseEnvFlag flag rest
parseEnv (['-',flag]:rest) = parseEnvFlag [flag] rest
parseEnv (other:rest) = mempty{errors = ["Unknown argument: " <> other]} <> parseEnv rest

parseEnvFlag :: String -> [String] -> EnvContent
parseEnvFlag flag rest =
    case rest of
    [] -> flagRes
    ('-':_):_ -> flagRes <> parseEnv rest
    val:rest' -> mempty{options = M.singleton flag val} <> parseEnv rest'
    where
        flagRes = mempty{flags = S.singleton flag}

-- | A boolean flag which may be initialized by an environment variable.
--
-- If the flag is present in the environment,
-- the corresponding --no-<name> or --<name> flag will be available to override it.
envSwitch :: EnvOpts -> String -> Bool -> String -> O.Parser Bool
envSwitch envOpts name def desc =
    (/= curDef) <$> O.switch (O.long flag <> O.help help)
    where
        flag
            | otherInEnv = defaultMode
            | otherwise = otherMode
        help = actionHelp <> " " <> desc <> extraHelp
        actionHelp
            | curDef = "Disable"
            | otherwise = "Enable"
        extraHelp = guard otherInEnv >> overrideHelp envOpts otherMode
        curDef = def /= otherInEnv
        noFlag = "no-" <> name
        (defaultMode, otherMode)
            | def = (name, noFlag)
            | otherwise = (noFlag, name)
        otherInEnv = S.member otherMode envOpts.content.flags

overrideHelp :: EnvOpts -> String -> String
overrideHelp envOpts val = " (override \"--" <> val <> "\" from " <> envOpts.envVarName <> ")"

-- | An optional value which may be initialized by an environment variable.
--
-- If the flag is present in the environment,
-- a corresponding --no-<name> flag will be available to disable it.
envOptional :: (Read a, Show a) => EnvOpts -> String -> String -> String -> (a -> String) -> O.Parser (Maybe a)
envOptional envOpts name valDesc help disableHelp =
    case M.lookup name envOpts.content.options >>= readMaybe of
    Just val ->
        O.optional (
            envOption envOpts name Nothing (commonMods <> envOptionFromEnv envOpts val)
        ) <|> f <$> O.switch (O.long ("no-" <> name) <> O.help h)
        where
            h = disableHelp val <> overrideHelp envOpts (name <> " " <> show val)
            f True = Nothing
            f False = Just val
    Nothing -> O.optional (O.option O.auto (O.long name <> commonMods))
    where
        commonMods = O.metavar valDesc <> O.help help

-- | An option with a default value which may be initialized by an environment variable.
--
-- (the default value should be specified in the provided @O.Mod@ argument)
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
