-- | Expose cabal package version as a string
module Version (versionString) where

import Paths_git_mediate (version)
import Data.Version (showVersion)

versionString :: String
versionString = showVersion version
