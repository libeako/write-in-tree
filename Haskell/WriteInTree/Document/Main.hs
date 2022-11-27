module WriteInTree.Document.Main
(
	Document (..),
	core_in_document, node_in_document,
)
where

import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude
import WriteInTree.Document.Core.Data
import WriteInTree.Document.SepProps.Data (DocSepProps)

import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Data as Data


data Document =
	Document
	{ docSepProps :: DocSepProps
	, docCore :: Site
	}
	deriving (Eq)


core_in_document :: Optic.Lens' Site Document
core_in_document = Optic.lens_from_get_set docCore (\ c (Document sp _) -> Document sp c)

node_in_document :: Optic.Traversal' Data.Node Document
node_in_document = Category2.identity >**>^ node_in_site >**>^ core_in_document
