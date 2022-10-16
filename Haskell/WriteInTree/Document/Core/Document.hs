-- | The file containing the design intentions of the user, that is the document.
module WriteInTree.Document.Core.Document where


import Fana.Prelude

import qualified WriteInTree.Document.Core.Serial.Page.Tree as Page


data Document (id_u :: Type) =
	Document
	{
		docSite :: Page.Site id_u
	}
	deriving (Eq)
