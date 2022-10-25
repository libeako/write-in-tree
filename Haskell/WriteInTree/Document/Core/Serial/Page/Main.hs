module WriteInTree.Document.Core.Serial.Page.Main
(
	SubPageTarget (..),
	CrossLinkTarget (..),
	LinkInternalTarget (..),
	Link, Inline, Paragraph, Node, Structure,
	Page (..),
	PageKey, Site (..),
	get_page_of_Site_at, get_CrossLinkTarget_page,

	title_of_section, title_of_page, is_inline_a_page_break, page_addresses_in_site, text_content_in_site,
	node_in_site,

	layer,
)
where

import Data.Tree (Tree)
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import WriteInTree.Document.Core.Serial.Page.Data
import WriteInTree.Document.Core.Serial.Page.Tree (layer)

import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Data as Basic
import qualified WriteInTree.Document.Core.Serial.Page.BreakStructure as BS
import qualified WriteInTree.Document.Core.Serial.Page.Count as Count


layer' :: Optic.Iso' (Tree (Basic.Node i)) (BS.Page Count.Ordinal (Basic.Node i))
layer' =
	Category2.identity
	>**>^ BS.layer
	>**>^ Count.layer
