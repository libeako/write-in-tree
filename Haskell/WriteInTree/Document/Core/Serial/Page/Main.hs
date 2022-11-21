module WriteInTree.Document.Core.Serial.Page.Main
(
	Page, Site,
	title_of_section, title_of_page, text_content_in_site,
	node_in_site,
	layer,
)
where

import Data.Tree (Tree)
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import WriteInTree.Document.Core.Data (Paragraph)
import WriteInTree.Document.Core.Serial.Page.Data
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (Labels)
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (..))

import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Serial.Page.Border as Border
import qualified WriteInTree.Document.Core.Serial.Page.Serialize as Serialize


type ParseError = Serialize.ParseError


layer ::
	Optic.PartialIso' ParseError 
		(Tree (Labels, Positioned Paragraph)) Page
layer =
	Category2.identity
	>**>^ Border.layer
	>**>^ Serialize.layer
