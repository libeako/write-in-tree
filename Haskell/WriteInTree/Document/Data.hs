module WriteInTree.Document.Data
(
	Data (..), core_in_document, page_addresses_in_document,
)
where

import Fana.Prelude

import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..))
import WriteInTree.Document.SepProps.Data (DocSepProps)

import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Document as Core


data Data i =
	Data
	{ doc_sep_props :: DocSepProps
	, doc_core :: Core.Document i
	}
	deriving (Eq)


core_in_document :: Optic.Lens (Core.Document i1) (Core.Document i2) (Data i1) (Data i2)
core_in_document = Optic.lens_from_get_set doc_core (\ c (Data sp _) -> Data sp c)

page_addresses_in_document :: Optic.Traversal' (Maybe PageAddress) (Data i)
page_addresses_in_document =
	Category2.identity >**>^
	Core.page_addresses_in_doc >**>^ core_in_document
