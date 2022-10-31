module WriteInTree.Document.Core.Serial.Layers
(
	layer, layer_test,
)
where


import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude

import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified Technical.TextTree.MindMap as Tt
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.Link.InTree as Link
import qualified WriteInTree.Document.Core.Serial.Page.Main as Page
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNodeTextStructure as Mtt
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Serialize as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Path as Path
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos
import qualified WriteInTree.Document.Main as Data
import qualified WriteInTree.Document.SepProps.Data as SepProps


type Text = Base.String

convert_string_error :: Fana.Showable Text s => s -> Pos.PositionedMb (Accu.Accumulated Text)
convert_string_error = Fana.show >>> Pos.PositionedMb Nothing

type StructureAsTree = Data.StructureAsTree (Page.LinkInternalTarget Text)
type Document = Data.Document Text

type StructureAsTreeRaw = Data.StructureAsTree Text

layer_meta_text_escapee :: Optic.Iso' (Page.Site i) (Page.Site i)
layer_meta_text_escapee =
	Optic.lift_iso_by_function (Optic.fn_up Page.text_content_in_site) Mtt.layer_escapee

layer ::
	SepProps.DocSepProps -> 
	Optic.PartialIso' (Pos.PositionedMb (Accu.Accumulated Text)) Text (Page.Site Text)
layer sep_props = 
	Category2.identity
	>**>^ Optic.piso_convert_error convert_string_error Tt.layer 
	>**>^ Path.layer
	>**>^ Label.layer (SepProps.prop_inline_classes sep_props)
	>**>^ Optic.piso_convert_error Pos.maybefy_positioned Link.layer
	>**>^ Page.layer
	>**>^ layer_meta_text_escapee

layer_test :: Optic.PartialIso' (Pos.PositionedMb (Accu.Accumulated Text)) Text (Page.Site Text)
layer_test = layer def
