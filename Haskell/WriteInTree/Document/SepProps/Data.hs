module WriteInTree.Document.SepProps.Data
(
	DocSepProps (..), FolderSepProps (..),
	lang_ver_in_props, address_in_props,
)
where

import Prelude (Eq)
import Data.Default.Class
import WriteInTree.Document.Core.Data (PageAddress (..))

import qualified Fana.Optic.Concrete.Categories.Lens as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.LanguageVersion as Ver


type Text = Base.String

-- | Separate properties of a document.
data DocSepProps = DocSepProps
	{ language_version :: Ver.Version
	}
	deriving Eq

-- | Separate properties of a source folder.
data FolderSepProps =
	FolderSepProps
	{
		{-| Address of the page. -}
		address :: PageAddress
	}
	deriving Eq

instance Default DocSepProps where def = DocSepProps def
instance Default FolderSepProps where def = FolderSepProps (PageAddress def)

lang_ver_in_props :: Optic.Lens' Ver.Version DocSepProps
lang_ver_in_props = Optic.lens_from_get_set language_version (\ e c -> c { language_version = e })

address_in_props :: Optic.Lens' PageAddress FolderSepProps
address_in_props = Optic.lens_from_get_set address (\ e c -> c { address = e })
