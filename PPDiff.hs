module PPDiff (ppDiff) where

import Data.Algorithm.Diff (Diff(..))
import System.Console.ANSI

wrap :: Color -> String -> String
wrap color str = setSGRCode [SetColor Foreground Vivid color] ++ str ++ setSGRCode [Reset]

ppDiff :: Diff String -> String
ppDiff (First x)  = wrap Red   $ '-':x
ppDiff (Second x) = wrap Green $ '+':x
ppDiff (Both x _) =              ' ':x
