module WriteInTree.Document.Data
(
	Data (..),
)
where

import Fana.Prelude

import WriteInTree.Document.SepProps.Data (DocSepProps)
import qualified WriteInTree.Document.Core.Document as Core


data Data id_u ia = Data
	{ doc_sep_props :: DocSepProps
	, doc_core :: Core.Document id_u 
	}
	deriving (Eq)
