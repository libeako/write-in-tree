module WriteInTree.Document.Core.Serial.Layers
(
	layer,
	layer_test,
)
where

import Data.Tree (Tree)
import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude

import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified Technical.TextTree.Data as Tt
import qualified Technical.TextTree.MindMap as Tt
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.InlineContent as InlineContent
import qualified WriteInTree.Document.Core.Serial.Link.InTree as Link
import qualified WriteInTree.Document.Core.Serial.Paging as Page
import qualified WriteInTree.Document.Core.Serial.Paragraph as Paragraph
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Path as Path
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos
import qualified WriteInTree.Document.Core.Serial.UserIdentifiers as UserIds
import qualified WriteInTree.Document.SepProps.Data as SepProps


type Text = Base.String

convert_string_error :: Fana.Showable Text s => s -> Pos.PositionedMb (Accu.Accumulated Text)
convert_string_error = Fana.show >>> Pos.PositionedMb Nothing

type DataWithConcreteParams t a = t (a ()) (a ()) Data.NodeIdU Data.NodeIdU Text
type StructureAsTree a = DataWithConcreteParams Data.StructureAsTree a
type Document a = DataWithConcreteParams Data.Document a

layer_document :: Optic.Iso' (StructureAsTree a) (Document a)
layer_document = Optic.Iso Data.docTree Data.Document

layer_rtt :: Optic.Iso' (Tree Tt.Elem') (Tree Path.ElemHET)
layer_rtt = convert_from_describing_class_4 Path.layer

layer ::
	SepProps.DocSepProps -> 
	Optic.PartialIso' (Pos.PositionedMb (Accu.Accumulated Text)) Text (Document (Label.Elem Text))
layer sep_props = 
	Category2.empty
	>**>^ Optic.piso_convert_error convert_string_error Tt.layer 
	>**>^ layer_rtt
	>**>^ Optic.piso_convert_error (Pos.PositionedMb Nothing) (Label.layer (SepProps.prop_inline_classes sep_props))
	>**>^ Optic.piso_convert_error Pos.maybefy_positioned InlineContent.layer
	>**>^ Optic.piso_convert_error Pos.maybefy_positioned Link.layer
	>**>^ Paragraph.layer
	>**>^ Page.layer
	>**>^ Optic.piso_convert_error Pos.maybefy_positioned UserIds.layer
	>**>^ layer_document

layer_test :: Optic.PartialIso' (Pos.PositionedMb (Accu.Accumulated Text)) Text (Document (Label.Elem Text))
layer_test = layer def
