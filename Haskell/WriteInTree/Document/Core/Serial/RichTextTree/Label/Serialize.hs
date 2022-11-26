module WriteInTree.Document.Core.Serial.RichTextTree.Label.Serialize
(
	serialize_forest,
)
where

import Data.Tree (Forest)
import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (Positioned))

import qualified Fana.Data.HasSingle as HasSingle
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Char = Base.Char
type Text = [Char]
type ElemLR = Text
type ElemP = Positioned
type ElemPT = ElemP Text
type ElemT = Positioned Text


parse_node :: ElemPT -> ElemT
parse_node node = Positioned (Pos.get_position node) (HasSingle.elem node)

serialize_node :: Optic.Iso ElemLR ElemPT Text ElemT
serialize_node = Optic.Iso id parse_node

serialize_forest :: Optic.Iso (Forest ElemLR) (Forest ElemPT) (Forest Text) (Forest ElemT)
serialize_forest = (Optic.lift_iso >>> Optic.lift_iso) serialize_node
