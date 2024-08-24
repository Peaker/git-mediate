{-# LANGUAGE OverloadedRecordDot, LambdaCase, FlexibleContexts #-}

module OptUtils
    ( Parser, parseEnvOptions, envSwitch, envOptional, envOption
    ) where

import           Control.Applicative ((<|>))
import           Control.Monad ((>=>), guard, unless)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor.Compose (Compose(..))
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import qualified Data.Set as S
import           Data.Foldable (traverse_)
import qualified Options.Applicative as O
import           System.Environment (lookupEnv)
import           Text.Read.Compat (readMaybe)

type Parser = Compose (ReaderT EnvVarName (State EnvContent)) O.Parser

type EnvVarName = String

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

parseEnvOptions :: String -> Parser a -> IO (O.Parser a)
parseEnvOptions name (Compose parser) =
    do
        envOpts <- foldMap (parseEnv . words) <$> lookupEnv name
        let (result, remainder) = runState (runReaderT parser name) envOpts
        let errFmt = formatRemainder remainder
        result <$
            unless (null errFmt)
            (putStrLn (unlines
                (("Warning: unhandled options in " <> name <> ":") : (("  * " <>) <$> errFmt))))

formatRemainder :: EnvContent -> [String]
formatRemainder (EnvContent f o e) =
    (("Unrecognized flag --" <>) <$> S.toList f)
    <> M.foldMapWithKey (\k v -> ["Unrecognized option: --" <> k <> " " <> v]) o
    <> e

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
envSwitch :: String -> Bool -> String -> Parser Bool
envSwitch name def desc =
    Compose $
    do
        otherInEnv <- gets (S.member otherMode . flags)
        modify (\x -> x{flags = S.delete otherMode x.flags})
        let flag
                | otherInEnv = defaultMode
                | otherwise = otherMode
        let curDef = def /= otherInEnv
        let actionHelp
                | curDef = "Disable"
                | otherwise = "Enable"
        extraHelp <- (guard otherInEnv >>) <$> overrideHelp otherMode
        let help = actionHelp <> " " <> desc <> extraHelp
        pure ((/= curDef) <$> O.switch (O.long flag <> O.help help))
    where
        noFlag = "no-" <> name
        (defaultMode, otherMode)
            | def = (name, noFlag)
            | otherwise = (noFlag, name)

overrideHelp :: MonadReader EnvVarName m => String -> m String
overrideHelp val = asks (\name -> " (override \"--" <> val <> "\" from " <> name <> ")")

-- | An optional value which may be initialized by an environment variable.
--
-- If the flag is present in the environment,
-- a corresponding --no-<name> flag will be available to disable it.
envOptional :: (Read a, Show a) => String -> String -> String -> (a -> String) -> Parser (Maybe a)
envOptional name valDesc help disableHelp =
    Compose $
    readOption name >>=
    \case
    Just val ->
        do
            oh <- overrideHelp (name <> " " <> show val)
            ov <- envOptionFromEnv val
            opt <- getCompose (envOption name Nothing (commonMods <> ov))
            pure $
                O.optional opt
                <|> f <$> O.switch (O.long ("no-" <> name) <> O.help (disableHelp val <> oh))
        where
            f True = Nothing
            f False = Just val
    Nothing -> pure (O.optional (O.option O.auto (O.long name <> commonMods)))
    where
        commonMods = O.metavar valDesc <> O.help help

readOption :: (Read a, MonadState EnvContent m) => String -> m (Maybe a)
readOption name =
    do
        result <- gets (M.lookup name . options >=> readMaybe)
        result <$ traverse_ (const (modify (\x -> x{options = M.delete name x.options}))) result

-- | An option with a default value which may be initialized by an environment variable.
--
-- (the default value should be specified in the provided @O.Mod@ argument)
envOption :: (Read a, Show a) => String -> Maybe Char -> O.Mod O.OptionFields a -> Parser a
envOption name shortName mods =
    Compose $
    traverse readOption ([name] <> maybe [] (pure . pure) shortName)
    >>=
    \case
    [] -> pure (O.option O.auto baseMods)
    [val] -> O.option O.auto . (baseMods <>) <$> envOptionFromEnv val
    _ ->
        O.option O.auto baseMods <$
        modify (\x -> x{errors = x.errors <> ["Both --" <> name <> " and -" <> maybe [] pure shortName <> " specified"]})
    . catMaybes
    where
        baseMods = O.long name <> foldMap O.short shortName <> mods

envOptionFromEnv :: (MonadReader EnvVarName m, Show a) => a -> m (O.Mod O.OptionFields a)
envOptionFromEnv val =
    asks (\envVar -> O.value val <> O.showDefaultWith (\x -> show x <> ", from " <> envVar))
