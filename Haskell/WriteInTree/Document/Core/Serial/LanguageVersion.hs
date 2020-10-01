module WriteInTree.Document.Core.Serial.LanguageVersion where


import Control.Monad ((>=>))
import Data.Default.Class
import Fana.Prelude
import Prelude ((==))

import qualified Control.Monad as Monad
import qualified Data.Int as Int
import qualified Fana.Data.List as List
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Serial.Print.Show as Fana
import qualified Fana.Serial.Print.Show as Show
import qualified Prelude as Base


type Text = Base.String

type VersionNumber = Int.Int64
data Version = Version
	{
		-- Backward compatibility breaks.
		verGeneration :: VersionNumber,
		-- Backward compatibility is intended to be fully kept.
		verImprovement :: VersionNumber
	}
	deriving Eq
current :: Version
current = Version
	{
		verGeneration = 1,
		verImprovement = 1
	}

instance Default Version where def = current

separator_char :: Base.Char
separator_char = '.'

instance Fana.Showable Text Version where
	show v = Show.from_Show (verGeneration v) <> Show.from_Char '.' <> Show.from_Show (verImprovement v)

data ParseError = 
	  PeNotSupported
	| PeRootHasNoChildren
	| PeNumberOfChildrenOfLanguageNodeIsNot2
	| PeWrongLanguageName
	| PeVersionHasNot2Numbers
	| PeNotInteger Text

instance Fana.Showable Text ParseError where
	show = \case
		PeNotSupported -> "This version of the language is not supported."
		PeRootHasNoChildren -> "The root node of the tree has no children, not even a language node."
		PeNumberOfChildrenOfLanguageNodeIsNot2 -> "A language node must have exactly 2 children."
		PeWrongLanguageName -> "The language name is not write-in-tree."
		PeVersionHasNot2Numbers -> "The version must contain at least 2 numbers separated by dots."
		PeNotInteger t -> "Language version error: " <> Accu.single t <> " is not an integer."

int_from_text :: Text -> Either ParseError VersionNumber
int_from_text t = 
	case (Base.reads @VersionNumber) t of
		[(i, "")] -> Right i
		_ -> Left (PeNotInteger t)

version_from_numbers :: [VersionNumber] -> Either ParseError Version
version_from_numbers = \case
	(gen:imp:_) -> Right (Version { verGeneration = gen, verImprovement = imp })
	_ -> Left PeVersionHasNot2Numbers

version_from_text :: Text -> Either ParseError Version
version_from_text = Base.id
	>>> List.split_where (== separator_char)
	>>> Monad.mapM int_from_text
	>=> version_from_numbers

-- | Checks whether the given version is supported.
language_error :: Version -> Maybe ParseError
language_error v = if (verGeneration v) == 1
	then Nothing
	else Just PeNotSupported
