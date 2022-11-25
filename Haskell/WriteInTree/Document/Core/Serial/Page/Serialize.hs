module WriteInTree.Document.Core.Serial.Page.Serialize
(
	layer,
)
where

import Data.Tree (Forest)
import Fana.Prelude
import WriteInTree.Document.Core.Data
import WriteInTree.Document.Core.Serial.Page.Data (PageContentBulk)

import qualified Fana.Optic.Concrete.Prelude as Optic


layer :: Optic.Iso (Forest Node) (Forest Node) PageContentBulk PageContentBulk
layer = Optic.Iso id id
