module WriteInTree.Document.Core.Serial.Page.Border
(
	serialize,
)
where

import Data.Tree (Forest)
import Fana.Prelude
import WriteInTree.Document.Core.Data (Node, nodeLabels, nodePosition, nodeContent, Paragraph)
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (Labels)
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (..))

import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Data as Data


render_from_node :: Node -> (Labels, Positioned Paragraph)
render_from_node node = (nodeLabels node, Positioned (nodePosition node) (nodeContent node))

parse_into_node :: (Labels, Positioned Paragraph) -> Node
parse_into_node (l, Positioned pos par) = Data.Node pos l par

serialize ::
	Optic.Iso
		(Forest (Labels, Positioned Paragraph))
		(Forest (Labels, Positioned Paragraph))
		(Forest Node) (Forest Node)
serialize = (Optic.lift_iso >>> Optic.lift_iso) (Optic.Iso render_from_node parse_into_node)
