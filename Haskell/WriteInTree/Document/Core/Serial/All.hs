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

import qualified Data.Bifunctor as BiFr
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

loose_meta' :: Optic.PartialIso' Text (Positioned (Inline InNode.Structure)) (Positioned InlineT)
loose_meta' =
	let
		render :: InlineT -> Inline InNode.Structure
		render = map InNode.Norm
		parse'' :: InNode.Structure -> Either Text Text
		parse'' =
			\case
				InNode.Norm t -> Right t
				InNode.Meta t -> Left ("could not recognize meta text \"" <> t <> "\"")
		parse' :: Inline InNode.Structure -> Either Text InlineT
		parse' = traverse parse''
		parse :: Positioned (Inline InNode.Structure) -> Either Text (Positioned InlineT)
		parse x = BiFr.first (prefix_error_message_with_position_from x) (traverse parse' x)
		in Optic.PartialIso (map render) parse

loose_meta ::
	Optic.PartialIso' Text
		(Forest (Positioned (Inline InNode.Structure)))
		(Forest (Positioned InlineT))
loose_meta = (Optic.lift_piso >>> Optic.lift_piso) loose_meta'

serialize :: Optic.PartialIso' Text Text PageContentBulk
serialize =
	Category2.identity
	>**> Tt.text_tree
	>**> Optic.to_PartialIso positioning
	>**> node
	>**> Optic.to_PartialIso Link.serialize
	>**> loose_meta
	>**> Optic.to_PartialIso Node.serialize
	>**> Optic.to_PartialIso meta_text_escape
