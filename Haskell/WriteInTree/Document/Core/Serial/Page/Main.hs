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
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (Labels)
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (..))

import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Data as Basic
import qualified WriteInTree.Document.Core.Serial.Page.Address as Address
import qualified WriteInTree.Document.Core.Serial.Page.Border as Border
import qualified WriteInTree.Document.Core.Serial.Page.BreakStructure as BS
import qualified WriteInTree.Document.Core.Serial.Page.Count as Count
import qualified WriteInTree.Document.Core.Serial.Page.Cut as Cut
import qualified WriteInTree.Document.Core.Serial.Page.SiteStructureDiscovery as SS


type ParseError = Address.ParseError


layer ::
	Optic.PartialIso' ParseError
		(Tree (Labels, Positioned (Basic.Paragraph Text))) 
		(Site Text)
layer =
	Category2.identity
	>**>^ Border.layer
	>**>^ BS.layer
	>**>^ Count.layer
	>**>^ SS.layer
	>**>^ Address.layer
	>**>^ Cut.layer
