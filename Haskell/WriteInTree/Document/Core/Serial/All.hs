module WriteInTree.Document.Core.Serial.All
(
	serialize,
)
where

import Data.Tree (Forest)
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Math.Algebra.Monoid.Accumulate (Accumulated)
import Fana.Prelude
import WriteInTree.Document.Core.Data
import WriteInTree.Document.Core.Serial.Position

import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Instances.Text.Indent as Tt
import qualified Fana.Serial.Print.Show as Fana
import qualified WriteInTree.Document.Core.Serial.InNodeTextStructure as Mtt
import qualified WriteInTree.Document.Core.Serial.Link.InTree as Link
import qualified WriteInTree.Document.Core.Serial.Node as Node
import qualified WriteInTree.Document.Core.Serial.Position as Pos


show_error :: Fana.Showable Text e => e -> PositionedMb (Accumulated Text)
show_error = Fana.show >>> PositionedMb Nothing

meta_text_escape :: Optic.Iso' PageContentBulk PageContentBulk
meta_text_escape =
	Optic.lift_iso_by_function (Optic.fn_up text_content_in_page_content_bulk) Mtt.layer_escapee

positioning :: Optic.Iso (Forest Text) (Forest (Count, Text)) (Forest Text) (Forest (Positioned Text))
positioning = Optic.Iso id (map (map (uncurry Positioned)))

serialize :: Optic.PartialIso' (PositionedMb (Accumulated Text)) Text PageContentBulk
serialize =
	Category2.identity
	>**>^ Optic.piso_convert_error show_error Tt.text_tree
	>**>^ positioning
	>**>^ Optic.piso_convert_error Pos.maybefy_positioned Link.serialize
	>**>^ Node.serialize
	>**>^ meta_text_escape
