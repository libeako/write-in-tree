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
import qualified WriteInTree.Document.Core.Serial.Id.Forest as Id
import qualified WriteInTree.Document.Core.Serial.InNodeTextStructure as Mtt
import qualified WriteInTree.Document.Core.Serial.Link.InTree as Link
import qualified WriteInTree.Document.Core.Serial.Node as Node
import qualified WriteInTree.Document.Core.Serial.InNodeTextStructure as InNode


meta_text_escape :: Optic.Iso' StructureAsForest StructureAsForest
meta_text_escape =
	Optic.lift_iso_by_function (Optic.fn_up text_content_in_PageContentBulk) Mtt.layer_escapee

positioning :: Optic.Iso (Forest Text) (Forest (Count, Text)) (Forest Text) (Forest (Positioned Text))
positioning = Optic.Iso id (map (map (uncurry Positioned)))

type CollectionSerializer c l h =
	Optic.PartialIso Text
		(c l) (c (Positioned l))
		(c h) (c (Positioned h))

node :: CollectionSerializer Forest Text InNode.Structure
node = Optic.PartialIso ((map >>> map) InNode.render) ((traverse >>> traverse >>> traverse) InNode.parse)

-- ~ serialize_id :: ForestSerializer InNode.Structure (Address, InNode.Structure)

loose_meta' :: 
	Optic.PartialIso Text
		(Paragraph InNode.Structure)
		(Positioned (Paragraph InNode.Structure))
		(Positioned ParagraphT)
		(Positioned ParagraphT)
loose_meta' =
	let
		render :: ParagraphT -> Paragraph InNode.Structure
		render = map InNode.Norm
		parse'' :: InNode.Structure -> Either Text Text
		parse'' =
			\case
				InNode.Norm t -> Right t
				InNode.Meta t -> Left ("could not recognize meta text \"" <> t <> "\"")
		parse' :: Inline InNode.Structure -> Either Text InlineT
		parse' = traverse parse''
		parse :: Positioned (Paragraph InNode.Structure) -> Either Text (Positioned ParagraphT)
		parse x = BiFr.first (prefix_error_message_with_position_from x) (traverse parse' x)
		in Optic.PartialIso (positionedValue >>> render) parse

loose_meta ::
	Optic.PartialIso Text
		(ForestA (Paragraph InNode.Structure))
		(ForestA (Positioned (Paragraph InNode.Structure)))
		(ForestA (Positioned ParagraphT))
		(ForestA (Positioned ParagraphT))
loose_meta = (Optic.lift_piso >>> Optic.lift_piso >>> Optic.lift_piso) loose_meta'

serialize :: Optic.PartialIso' Text Text StructureAsForest
serialize =
	Category2.identity
	>**> Tt.text_tree
	>**> Optic.to_PartialIso positioning
	>**> node
	>**> Id.serialize
	>**> Optic.to_PartialIso Link.serialize
	>**> loose_meta
	>**> Optic.to_PartialIso Node.serialize
	>**> Optic.to_PartialIso meta_text_escape
