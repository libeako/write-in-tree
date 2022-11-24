module WriteInTree.Document.Core.Serial.Page.Border
(
	layer,
)
where

import Data.Tree (Tree, Forest)
import Fana.Prelude
import WriteInTree.Document.Core.Data (Node, nodeLabels, nodePosition, nodeContent, Paragraph)
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (Labels)
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (..))

import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.ClassPrefix as Class


type Text = Base.String

-- | text value of node class signalling the separate page status
text_page_class :: Text
text_page_class = Class.class_prefix <> "page"

render_from_node :: Node -> (Labels, Positioned Paragraph)
render_from_node node = (nodeLabels node, Positioned (nodePosition node) (nodeContent node))

parse_into_node :: (Labels, Positioned Paragraph) -> Node
parse_into_node (l, Positioned pos par) = Data.Node pos l par

layer :: Optic.Iso (Forest (Labels, Positioned Paragraph)) (Tree (Labels, Positioned Paragraph)) (Forest Node) (Tree Node)
layer = Optic.change_iso_per_component map id (Optic.lift_iso (Optic.Iso render_from_node parse_into_node))
