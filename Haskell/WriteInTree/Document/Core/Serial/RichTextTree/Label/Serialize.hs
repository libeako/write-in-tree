module WriteInTree.Document.Core.Serial.RichTextTree.Label.Serialize
(
	LabeledPositioned (..),
	serialize_forest,
)
where

import Data.Tree (Forest)
import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (Labels)
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (Positioned))

import qualified Fana.Data.HasSingle as HasSingle
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure as Structure
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Char = Base.Char
type Text = [Char]
type ElemLR = Text
type ElemP = Positioned
type ElemPT = ElemP Text
type LabeledPositioned e = (Labels, Positioned e)
type ElemT = LabeledPositioned Text

meta_name_class :: Text
meta_name_class = "class"

render_node :: ElemT -> ElemLR
render_node = HasSingle.elem >>> HasSingle.elem

parse_node :: ElemPT -> ElemT
parse_node node =
	let
		position = Pos.get_position node
		labels = Structure.Labels
		in (labels, (Positioned position (HasSingle.elem node)))

serialize_node :: Optic.Iso ElemLR ElemPT ElemT ElemT
serialize_node = Optic.Iso render_node parse_node

serialize_forest :: Optic.Iso (Forest ElemLR) (Forest ElemPT) (Forest ElemT) (Forest ElemT)
serialize_forest = (Optic.lift_iso >>> Optic.lift_iso) serialize_node
