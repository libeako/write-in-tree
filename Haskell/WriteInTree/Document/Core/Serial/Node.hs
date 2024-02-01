module WriteInTree.Document.Core.Serial.Node
(
	serialize,
)
where

import WriteInTree.Document.Core.Data
import WriteInTree.Document.Core.Serial.Position (Positioned (..))

import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Data as Data


render_from_node :: Node -> Positioned ParagraphT
render_from_node node = Positioned (nodePosition node) (nodeContent node)

parse_into_node :: Positioned ParagraphT -> Node
parse_into_node (Positioned pos par) = Data.Node pos par

serialize ::
	Optic.Iso
		(ForestA (Positioned ParagraphT))
		(ForestA (Positioned ParagraphT))
		(ForestA Node) (ForestA Node)
serialize = (Optic.lift_iso) (Optic.Iso render_from_node parse_into_node)
