module WriteInTree.Document.SepProps.Data
(
	InlineClass (..), ofInlineClass_name, ofInlineClass_codes,
	DocSepProps (..), lens_lang_ver_in_props, ofProps_inline_classes,
)
where

import Prelude (Eq)
import Data.Default.Class

import qualified Fana.Optic.Concrete.Categories.Lens as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.LanguageVersion as Ver


type Text = Base.String

-- | represents a rule saying how to represent a class of a text tree element 
-- inside the text content of the element
data InlineClass =
	InlineClass
	{ ilc_name :: Text
	, ilc_codes :: [Text]
	}
	deriving (Eq)

ofInlineClass_name :: Optic.Lens' Text InlineClass
ofInlineClass_name = Optic.lens_from_get_set ilc_name (\ e c -> c { ilc_name = e })

ofInlineClass_codes :: Optic.Lens' [Text] InlineClass
ofInlineClass_codes = Optic.lens_from_get_set ilc_codes (\ e c -> c { ilc_codes = e })

instance Default InlineClass where def = InlineClass def def


-- | Separate properties of a document.
data DocSepProps = DocSepProps
	{ language_version :: Ver.Version
	, prop_inline_classes :: [InlineClass]
	}
	deriving Eq

instance Default DocSepProps where def = DocSepProps def def

lens_lang_ver_in_props :: Optic.Lens' Ver.Version DocSepProps
lens_lang_ver_in_props = Optic.lens_from_get_set language_version (\ e c -> c { language_version = e })

ofProps_inline_classes :: Optic.Lens' [InlineClass] DocSepProps
ofProps_inline_classes = Optic.lens_from_get_set prop_inline_classes (\ e c -> c { prop_inline_classes = e })
