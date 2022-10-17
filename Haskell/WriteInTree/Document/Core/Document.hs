-- | The file containing the design intentions of the user, that is the document.
module WriteInTree.Document.Core.Document where


import Fana.Prelude
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import WriteInTree.Document.Core.Serial.Page.Tree (Site)
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..))

import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Serial.Page.Tree as Page


data Document (i :: Type) =
	Document
	{
		docSite :: Site i
	}
	deriving (Eq)


site_in_doc :: Optic.Lens (Site i1) (Site i2) (Document i1) (Document i2)
site_in_doc = Optic.lens_from_get_set docSite (\ s (Document _) -> Document s)

page_addresses_in_doc :: Optic.Traversal' (Maybe PageAddress) (Document i)
page_addresses_in_doc =
		Category2.identity >**>^
		Page.page_addresses_in_site >**>^ site_in_doc
