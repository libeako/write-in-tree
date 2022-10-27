module WriteInTree.Document.Core.Serial.Page.Main
(
	SubPageTarget (..),
	CrossLinkTarget (..),
	LinkInternalTarget (..),
	Link, Inline, Paragraph, Node, Structure,
	Page (..),
	PageKey, SiteStructure (..), Site (..),
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
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..))

import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Data as Basic
import qualified WriteInTree.Document.Core.Serial.Page.Address as Address
import qualified WriteInTree.Document.Core.Serial.Page.BreakStructure as BS
import qualified WriteInTree.Document.Core.Serial.Page.Count as Count
import qualified WriteInTree.Document.Core.Serial.Page.SiteStructureDiscovery as SS

type ParseError = Address.ParseError

layer' ::
	Optic.PartialIso ParseError
		(Tree (Basic.Node i1)) (Tree (Basic.Node i2))
		(SiteStructure (PageAddress, BS.Page () (Basic.Node i1)))
		(SiteStructure (PageAddress, BS.Page PageKey (Basic.Node i2)))
layer' =
	Category2.identity
	>**>^ BS.layer
	>**>^ Count.layer
	>**>^ SS.layer
	>**>^ Address.layer
