module WriteInTree.Document.Core.Serial.Layers
(
	layer,
	layer_test,
)
where


import Data.Tree (Tree)
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude

import qualified Fana.Data.HasSingle as Fana
import qualified Fana.Data.HasSingle as HasSingle
import qualified Fana.Haskell.DescribingClass as Class
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified Technical.TextTree.MindMap as Tt
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Document as Data
import qualified WriteInTree.Document.Core.Serial.Link.InTree as Link
import qualified WriteInTree.Document.Core.Serial.Page.Border as PageBorder
import qualified WriteInTree.Document.Core.Serial.Page.Tree as Page
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.TextStructure as Mtt
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Path as Path
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos
import qualified WriteInTree.Document.Core.Serial.UserIdentifiers as UserIds
import qualified WriteInTree.Document.SepProps.Data as SepProps


type Text = Base.String

convert_string_error :: Fana.Showable Text s => s -> Pos.PositionedMb (Accu.Accumulated Text)
convert_string_error = Fana.show >>> Pos.PositionedMb Nothing

type StructureAsTree = Data.StructureAsTree Data.NodeIdU (Page.LinkInternalTarget Data.NodeIdU)
type Document = Data.Document Data.NodeIdU

type StructureAsTreeRaw = Data.StructureAsTree Data.NodeIdU Data.NodeIdU

layer_move_additional_info :: Fana.HasSingle a => Optic.Iso' (Tree (a e)) (Tree (a (), e))
layer_move_additional_info = Optic.lift_iso HasSingle.iso_separate

layer_meta_text_escapee ::
	Optic.PartialIso' (Pos.PositionedMb (Accu.Accumulated Text))
		StructureAsTreeRaw StructureAsTreeRaw
layer_meta_text_escapee =
	let
		iso_escapee :: Optic.Iso' StructureAsTreeRaw StructureAsTreeRaw
		iso_escapee = Optic.lift_iso_by_function (Optic.fn_up Data.texts_in_Tree) Mtt.layer_escapee
		in Class.convert_from_describing_class_4 iso_escapee

layer_document :: Optic.Iso' (Page.Site Data.NodeIdU) Document
layer_document = Optic.Iso Data.docSite Data.Document

layer ::
	SepProps.DocSepProps -> 
	Optic.PartialIso' (Pos.PositionedMb (Accu.Accumulated Text)) Text Document
layer sep_props = 
	Category2.identity
	>**>^ Optic.piso_convert_error convert_string_error Tt.layer 
	>**>^ Path.layer
	>**>^ Optic.piso_convert_error (Pos.PositionedMb Nothing) (Label.layer (SepProps.prop_inline_classes sep_props))
	>**>^ Optic.piso_convert_error Pos.maybefy_positioned Link.layer
	>**>^ layer_move_additional_info
	>**>^ PageBorder.layer
	>**>^ Optic.piso_convert_error Pos.maybefy_positioned UserIds.layer
	>**>^ layer_meta_text_escapee
	>**>^ Page.layer
	>**>^ layer_document

layer_test :: Optic.PartialIso' (Pos.PositionedMb (Accu.Accumulated Text)) Text Document
layer_test = layer def
