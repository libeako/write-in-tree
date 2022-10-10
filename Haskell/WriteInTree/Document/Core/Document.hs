-- | The file containing the design intentions of the user, that is the document.
module WriteInTree.Document.Core.Document where


import Data.Maybe (catMaybes)
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude
import WriteInTree.Document.Core.Data

import qualified Data.Foldable as Fold
import qualified Fana.Optic.Concrete.Prelude as Optic


data Document (id_u :: Type) ia =
	Document
	{
		docTree :: StructureAsTree id_u ia
	}
	deriving (Eq)


-- | collects all the user identifiers in the document with their nodes.
uids_int_doc_with_nodes :: Document id_u ia -> [(id_u, Node id_u ia)]
uids_int_doc_with_nodes = docTree >>> Fold.toList >>> map attach_its_uid_to_node >>> catMaybes


-- optics :

tree_in_Document ::
	Optic.Lens
		(StructureAsTree id_u1 ia1) (StructureAsTree id_u2 ia2)
		(Document id_u1 ia1) (Document id_u2 ia2)
tree_in_Document = Optic.lens_from_get_set docTree (\ p w -> w { docTree = p })

internal_address_in_document ::
	forall ia1 ia2 id_u .
	Optic.Traversal ia1 ia2 (Document id_u ia1) (Document id_u ia2)
internal_address_in_document = internal_address_in_node >**>^ node_in_tree >**>^ tree_in_Document
