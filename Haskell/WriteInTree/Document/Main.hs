module WriteInTree.Document.Main
(
	Document (..),
	core_in_document, page_addresses_in_doc,
)
where

import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude
import WriteInTree.Document.Core.Serial.Page.Tree (Site)
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..))
import WriteInTree.Document.SepProps.Data (DocSepProps)

import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Serial.Page.Tree as Page


data Document i =
	Document
	{ docSepProps :: DocSepProps
	, docCore :: Site i
	}
	deriving (Eq)


core_in_document :: Optic.Lens (Site i1) (Site i2) (Document i1) (Document i2)
core_in_document = Optic.lens_from_get_set docCore (\ c (Document sp _) -> Document sp c)

page_addresses_in_doc :: Optic.Traversal' (Maybe PageAddress) (Document i)
page_addresses_in_doc =
	Category2.identity
	>**>^ Page.page_addresses_in_site >**>^ core_in_document

node_in_doc :: Optic.Traversal' (Page.Node i) (Document i)
node_in_doc = Category2.identity >**>^ Page.node_in_site >**>^ core_in_document
