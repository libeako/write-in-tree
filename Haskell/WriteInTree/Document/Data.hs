module WriteInTree.Document.Data
(
	Data(..),
)
where

import WriteInTree.Document.SepProps.Data (DocSepProps)
import qualified WriteInTree.Document.Core.Data as Core


data Data al a id_u ia e = Data 
	{ doc_sep_props :: DocSepProps
	, doc_core :: Core.Document al a id_u ia e 
	}
