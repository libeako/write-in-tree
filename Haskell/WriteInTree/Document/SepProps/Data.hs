module WriteInTree.Document.SepProps.Data
(
	DocSepProps (..), lens_lang_ver_in_props,
)
where

import Prelude (Eq)
import Data.Default.Class

import qualified Fana.Optic.Concrete.Categories.Lens as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.LanguageVersion as Ver


type Text = Base.String

-- | Separate properties of a document.
data DocSepProps = DocSepProps
	{ language_version :: Ver.Version
	}
	deriving Eq

instance Default DocSepProps where def = DocSepProps def

lens_lang_ver_in_props :: Optic.Lens' Ver.Version DocSepProps
lens_lang_ver_in_props = Optic.lens_from_get_set language_version (\ e c -> c { language_version = e })
