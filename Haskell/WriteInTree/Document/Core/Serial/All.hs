module WriteInTree.Document.Core.Serial.All
(
	serialize,
)
where

import Data.Tree (Forest)
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import WriteInTree.Document.Core.Data
import WriteInTree.Document.Core.Serial.Position

import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Instances.Text.Indent as Tt
import qualified WriteInTree.Document.Core.Serial.InNodeTextStructure as Mtt
import qualified WriteInTree.Document.Core.Serial.Link.InTree as Link
import qualified WriteInTree.Document.Core.Serial.Node as Node
import qualified WriteInTree.Document.Core.Serial.InNodeTextStructure as InNode


meta_text_escape :: Optic.Iso' PageContentBulk PageContentBulk
meta_text_escape =
	Optic.lift_iso_by_function (Optic.fn_up text_content_in_page_content_bulk) Mtt.layer_escapee

positioning :: Optic.Iso (Forest Text) (Forest (Count, Text)) (Forest Text) (Forest (Positioned Text))
positioning = Optic.Iso id (map (map (uncurry Positioned)))

node :: 
	Optic.PartialIso Text
		(Forest Text) (Forest (Positioned Text))
		(Forest InNode.Structure) (Forest (Positioned InNode.Structure))
node = Optic.PartialIso ((map >>> map) InNode.render) ((traverse >>> traverse >>> traverse) InNode.parse)

serialize :: Optic.PartialIso' Text Text PageContentBulk
serialize =
	Category2.identity
	>**> Tt.text_tree
	>**> Optic.to_PartialIso positioning
	>**> node
	>**> Optic.to_PartialIso Link.serialize
	>**> Optic.to_PartialIso Node.serialize
	>**> Optic.to_PartialIso meta_text_escape
