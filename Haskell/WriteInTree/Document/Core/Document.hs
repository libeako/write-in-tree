-- | The file containing the design intentions of the user, that is the document.
module WriteInTree.Document.Core.Document where


import Data.Maybe (catMaybes)
import Fana.Prelude
import WriteInTree.Document.Core.Data

import qualified Data.Foldable as Fold
import qualified WriteInTree.Document.Core.Serial.Page.Tree as Page


data Document (id_u :: Type) =
	Document
	{
		docSite :: Page.Site id_u
	}
	deriving (Eq)


-- | collects all the user identifiers in the document with their nodes.
uids_int_doc_with_nodes :: Document id_u -> [(id_u, Node id_u id_u)]
uids_int_doc_with_nodes =
	tree_in_Document >>>
	Fold.toList >>> map attach_its_uid_to_node >>> catMaybes


-- optics :

tree_in_Document :: Document id_u -> StructureAsTree id_u id_u
tree_in_Document =
	docSite >>>
	Page.siteMainPage >>>
	Page.pageContent >>>
	Page.melt_pages_to_single
