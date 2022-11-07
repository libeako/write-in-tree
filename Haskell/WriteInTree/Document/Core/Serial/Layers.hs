module WriteInTree.Document.Core.Serial.Layers
(
	layer, layer_test,
)
where


import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Math.Algebra.Monoid.Accumulate (Accumulated)
import Fana.Prelude
import WriteInTree.Document.Core.Serial.Page.Main (Site)
import WriteInTree.Document.Core.Serial.RichTextTree.Position (PositionedMb (..))

import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified Technical.TextTree.MindMap as Tt
import qualified WriteInTree.Document.Core.Serial.Link.InTree as Link
import qualified WriteInTree.Document.Core.Serial.Page.Main as Page
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNodeTextStructure as Mtt
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Serialize as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Path as Path
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos
import qualified WriteInTree.Document.SepProps.Data as SepProps


type Text = Base.String

convert_string_error :: Fana.Showable Text s => s -> PositionedMb (Accumulated Text)
convert_string_error = Fana.show >>> PositionedMb Nothing

layer_meta_text_escapee :: Optic.Iso' Site Page.Site
layer_meta_text_escapee =
	Optic.lift_iso_by_function (Optic.fn_up Page.text_content_in_site) Mtt.layer_escapee

layer ::
	SepProps.DocSepProps -> 
	Optic.PartialIso' (PositionedMb (Accumulated Text)) Text Site
layer sep_props = 
	Category2.identity
	>**>^ Optic.piso_convert_error convert_string_error Tt.layer 
	>**>^ Path.layer
	>**>^ Label.layer (SepProps.prop_inline_classes sep_props)
	>**>^ Optic.piso_convert_error Pos.maybefy_positioned Link.layer
	>**>^ Page.layer
	>**>^ layer_meta_text_escapee

layer_test :: Optic.PartialIso' (PositionedMb (Accumulated Text)) Text Site
layer_test = layer def
