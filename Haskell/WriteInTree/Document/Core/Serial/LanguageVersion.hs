module WriteInTree.Document.Core.Serial.LanguageVersion
(
	Version,
	ParseError, version_from_text,
	language_is_supported,
)
where


import Control.Monad ()
import Fana.Prelude

import qualified Data.Int as Int
import qualified Prelude as Base


type Text = Base.String

type VersionNumber = Int.Int64
type Version = VersionNumber

data ParseError = 
	  PeNotSupported
	| PeNotInteger Text

instance Base.Show ParseError where
	show = \case
		PeNotSupported -> "This version of the language is not supported."
		PeNotInteger t -> "Language version error: " <> t <> " is not an integer."

int_from_text :: Text -> Either ParseError VersionNumber
int_from_text t = 
	case (Base.reads @VersionNumber) t of
		[(i, "")] -> Right i
		_ -> Left (PeNotInteger t)

version_from_text :: Text -> Either ParseError Version
version_from_text = int_from_text

language_is_supported :: Version -> Bool
language_is_supported = (== 1)

