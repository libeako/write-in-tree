module WriteInTree.Document.Core.Serial.Layers
(
	layer', layer, layer_test,
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
import qualified Fana.Serial.Print.Show as Fana
import qualified Technical.TextTree.General as Tt
import qualified Technical.TextTree.MindMap as Mm
import qualified WriteInTree.Document.Core.Serial.Link.InTree as Link
import qualified WriteInTree.Document.Core.Serial.Page.Main as Page
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNodeTextStructure as Mtt
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Serialize as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Path as Path
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos
import qualified WriteInTree.Document.SepProps.Data as SepProps


show_error :: Fana.Showable Text e => e -> PositionedMb (Accumulated Text)
show_error = Fana.show >>> PositionedMb Nothing

layer_meta_text_escapee' :: Optic.Iso' Page Page
layer_meta_text_escapee' =
	Optic.lift_iso_by_function (Optic.fn_up text_content_in_page) Mtt.layer_escapee

layer_meta_text_escapee :: Optic.Iso' Site Page.Site
layer_meta_text_escapee =
	Optic.lift_iso_by_function (Optic.fn_up Page.text_content_in_site) Mtt.layer_escapee

type LayerTextTree = Optic.PartialIso' (PositionedMb (Accumulated Text)) Text (Tree Text)

layer' ::
	Fana.Showable Text e =>
	SepProps.DocSepProps ->
	Optic.PartialIso' e Text [Tree Text] ->
	Optic.PartialIso' (PositionedMb (Accumulated Text)) Text Page
layer' sep_props text_tree_layer =
	Category2.identity
	>**>^ Optic.piso_convert_error show_error (Tt.forest_to_tree_serializer text_tree_layer)
	>**>^ Path.layer
	>**>^ Label.layer (SepProps.prop_inline_classes sep_props)
	>**>^ Optic.piso_convert_error Pos.maybefy_positioned Link.layer
	>**>^ Page.layer'
	>**>^ layer_meta_text_escapee'

layer ::
	Fana.Showable Text e =>
	SepProps.DocSepProps ->
	Optic.PartialIso' e Text [Tree Text] ->
	Optic.PartialIso' (PositionedMb (Accumulated Text)) Text Site
layer sep_props text_tree_layer =
	Category2.identity
	>**>^ Optic.piso_convert_error show_error (Tt.forest_to_tree_serializer text_tree_layer)
	>**>^ Path.layer
	>**>^ Label.layer (SepProps.prop_inline_classes sep_props)
	>**>^ Optic.piso_convert_error Pos.maybefy_positioned Link.layer
	>**>^ Page.layer
	>**>^ layer_meta_text_escapee

layer_test :: Optic.PartialIso' (PositionedMb (Accumulated Text)) Text Site
layer_test = layer def Mm.layer
