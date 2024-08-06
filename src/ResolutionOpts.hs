module ResolutionOpts
    ( ResolutionOptions(..), parser
    ) where

import qualified Options.Applicative as O

newtype ResolutionOptions = ResolutionOpts
    { untabify :: Maybe Int
    }

parser :: O.Parser ResolutionOptions
parser =
    ResolutionOpts
    <$> O.optional
        ( O.option O.auto
            ( O.long "untabify" <> O.metavar "TABSIZE"
                <> O.help "Convert tabs to the spaces at the tab stops for the given tab size"
            )
        )
