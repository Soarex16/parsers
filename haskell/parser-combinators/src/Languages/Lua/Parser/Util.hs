module Languages.Lua.Parser.Util where

import Languages.Lua.Grammar
import Data.Maybe
import Data.List.NonEmpty

mkNonEmpty c = c . fromJust . nonEmpty