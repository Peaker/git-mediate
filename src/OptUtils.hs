{-# LANGUAGE OverloadedRecordDot #-}

module OptUtils
    ( EnvOpts, parseEnv, envSwitch
    ) where

import           Data.Set (Set, fromList, member)
import qualified Options.Applicative as O

newtype EnvOpts = EnvOpts
    { flags :: Set String
    }

instance Semigroup EnvOpts where EnvOpts a <> EnvOpts b = EnvOpts (a <> b)
instance Monoid EnvOpts where mempty = EnvOpts mempty

parseEnv :: String -> EnvOpts
parseEnv env =
    EnvOpts $ fromList $ words env >>= unFlag
    where
        unFlag ('-':'-':flag) = [flag]
        unFlag _ = []

envSwitch :: EnvOpts -> String -> Bool -> String -> O.Parser Bool
envSwitch envOpts name def desc =
    (== def) <$> O.switch (O.long flag <> O.help help)
    where
        flag = if otherInEnv then defaultMode else otherMode
        help = (if curDef then "Disable " else "Enable ") <> desc
        curDef = def /= otherInEnv
        noFlag = "no-" <> name
        (defaultMode, otherMode) = if def then (name, noFlag) else (noFlag, name)
        otherInEnv = member otherMode envOpts.flags
