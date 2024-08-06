module ResolutionOpts
    ( ResolutionOptions(..), parser
    ) where

import qualified Options.Applicative as O

data ResolutionOptions = ResolutionOpts
    { untabify :: Maybe Int
    , lineEndings :: Bool
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
    <*> (not <$> O.switch (O.long "no-line-endings" <> O.help "Do not fix line ending characters conflict"))
