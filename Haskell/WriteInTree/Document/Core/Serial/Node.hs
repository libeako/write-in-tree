module WriteInTree.Document.Core.Serial.Node
(
	serialize,
)
where

import Data.Tree (Forest)
import Fana.Prelude
import WriteInTree.Document.Core.Data (Node, nodePosition, nodeContent, Paragraph)
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (..))

import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Data as Data


render_from_node :: Node -> Positioned Paragraph
render_from_node node = Positioned (nodePosition node) (nodeContent node)

parse_into_node :: Positioned Paragraph -> Node
parse_into_node (Positioned pos par) = Data.Node pos par

serialize ::
	Optic.Iso
		(Forest (Positioned Paragraph))
		(Forest (Positioned Paragraph))
		(Forest Node) (Forest Node)
serialize = (Optic.lift_iso >>> Optic.lift_iso) (Optic.Iso render_from_node parse_into_node)
