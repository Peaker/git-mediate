module PPDiff
  ( ColorEnable(..)
  , ppDiff
  ) where

import Data.Algorithm.Diff (Diff(..))
import System.Console.ANSI

data ColorEnable = EnableColor | DisableColor

wrap :: ColorEnable -> Color -> String -> String
wrap DisableColor _ str = str
wrap EnableColor color str = setSGRCode [SetColor Foreground Vivid color] ++ str ++ setSGRCode [Reset]

ppDiff :: ColorEnable -> Diff String -> String
ppDiff c (First x)  = wrap c Red   $ '-':x
ppDiff c (Second x) = wrap c Green $ '+':x
ppDiff _ (Both x _) =                ' ':x
