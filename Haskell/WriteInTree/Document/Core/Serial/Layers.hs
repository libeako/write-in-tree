module WriteInTree.Document.Core.Serial.Layers
(
	layer,
)
where

import Data.Tree (Tree)
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Math.Algebra.Monoid.Accumulate (Accumulated)
import Fana.Prelude
import WriteInTree.Document.Core.Serial.Page.Data
import WriteInTree.Document.Core.Serial.RichTextTree.Position (PositionedMb (..))

import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Instances.Text.Indent as Tt
import qualified Fana.Serial.Print.Show as Fana
import qualified WriteInTree.Document.Core.Serial.Link.InTree as Link
import qualified WriteInTree.Document.Core.Serial.Page.Border as PageBorder
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNodeTextStructure as Mtt
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Serialize as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Path as Path
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


show_error :: Fana.Showable Text e => e -> PositionedMb (Accumulated Text)
show_error = Fana.show >>> PositionedMb Nothing

layer_meta_text_escapee :: Optic.Iso' PageContentBulk PageContentBulk
layer_meta_text_escapee =
	Optic.lift_iso_by_function (Optic.fn_up text_content_in_page_content_bulk) Mtt.layer_escapee

type LayerTextTree = Optic.PartialIso' (PositionedMb (Accumulated Text)) Text (Tree Text)

layer :: Optic.PartialIso' (PositionedMb (Accumulated Text)) Text PageContentBulk
layer =
	Category2.identity
	>**>^ Optic.piso_convert_error show_error Tt.text_tree
	>**>^ Path.layer
	>**>^ Label.serialize_forest
	>**>^ Optic.piso_convert_error Pos.maybefy_positioned Link.layer
	>**>^ PageBorder.serialize
	>**>^ layer_meta_text_escapee
