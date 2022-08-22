module WriteInTree.Document.Core.Serial.Paging
(
	CoreHTree, CoreHTree', HTree, HTree',
	layer,
)
where

import Data.Tree (Tree)
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude

import qualified Data.Char as Base
import qualified Data.Foldable as Fold
import qualified Data.Tree as Tree
import qualified Fana.Data.Function as Fn
import qualified Fana.Data.Key.Map.Interface as Map
import qualified Fana.Data.Key.Map.KeyIsString as MapS
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.MetaStructure as Ms
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.ClassPrefix as Class
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String

type Paragraph = Data.Paragraph Text
type A = Label.Elem Text
type InputTree a = Tree (a (), Paragraph)

data MetaNodeName = MnLinksTo deriving (Base.Enum, Base.Bounded)

-- | the text value of a linked-contents meta node name
text_linked_contents :: Text
text_linked_contents = "linked-contents"

render_MetaNodeName :: MetaNodeName -> Text
render_MetaNodeName = \case { MnLinksTo -> text_linked_contents }

layer_MetaName :: Optic.Iso' (a (), Paragraph) (a (), Either MetaNodeName Paragraph)
layer_MetaName =
	Optic.lift_iso
		(
		Ms.serialize_node_content_without_worry
			(Data.ilVisual >>> Just)
			(flip Data.Inline Nothing)
			render_MetaNodeName
		)

-- | text value of node class signalling the separate page status
text_page_class :: Text
text_page_class = Class.class_prefix <> "page"

store_separate_page_status :: Bool -> Fn.Endo (Label.Elem id e)
store_separate_page_status separate_page = if separate_page
	then Optic.fn_up Label.inElem_labels (Label.add_new_classes_to_Labels [text_page_class])
	else id

type CoreLTree a = Tree (a (), Either MetaNodeName Paragraph)
type CoreHTree a = Tree (a (), (Paragraph, Bool))

core_render :: CoreHTree a -> CoreLTree a
core_render = (map >>> map) (fst >>> Right)

has_page_class :: Label.Elem id e -> Bool
has_page_class = Label.elem_has_class text_page_class

core_parse' :: forall a id . a ~ Label.Elem id =>
	Bool -> CoreLTree a -> [CoreHTree a]
core_parse' separate_page_as_inherited (Tree.Node (a, ei_paragraph) children) = let
	make_children :: Bool ->[CoreHTree a]
	make_children sp = Fold.concat (map (core_parse' sp) children)
	on_regular :: Paragraph -> [CoreHTree a]
	on_regular paragraph =
		[
			let
				update_additional_info = store_separate_page_status separate_page_as_inherited
				separate_page :: Bool
				separate_page = separate_page_as_inherited || has_page_class a
				new_trunk = (update_additional_info a, (paragraph, separate_page))
				in Tree.Node new_trunk (make_children False)
		]
	in Base.either (const (make_children True)) on_regular ei_paragraph

core_parse :: a ~ Label.Elem id => CoreLTree a -> Either (Accu.Accumulated Text) (CoreHTree a)
core_parse = core_parse' True >>> \case
	[single] -> Right single
	_ : _ : _ ->
		Left
			(
				mempty
				<> "the trunk may not be a \""
				<> Accu.single text_linked_contents
				<> "\" meta node"
			)
	_ -> Base.error "paging - parse - empty input tree"

core_layer :: a ~ Label.Elem id => Optic.PartialIso' (Accu.Accumulated Text) (CoreLTree a) (CoreHTree a)
core_layer = Optic.PartialIso core_render core_parse


type CoreHTree' = CoreHTree (Label.Elem Text)

type NodeH a = Data.Node (a ()) Text Text

parse_into_node ::
	forall a . a ~ Label.Elem Text =>
	(a (), (Paragraph, Bool)) -> Either (Pos.Positioned (Accu.Accumulated Text)) (NodeH a)
parse_into_node (a, (paragraph, is_separate_page)) = let
	make :: Text -> Data.Node (a ()) Text Text
	make id_a = Data.Node id_a a (a, paragraph) is_separate_page
	error_message :: Pos.Positioned (Accu.Accumulated Text)
	error_message = Pos.Positioned (Pos.get_position a) "node does not have an automatic identifier"
	in Base.maybe (Left error_message) Right (map make (Label.ofElem_auto_id a))

render_from_node :: NodeH a -> (a (), (Paragraph, Bool))
render_from_node i = (fst (Data.nodeContent i), (snd (Data.nodeContent i), Data.nodeIsSeparatePage i))

layer_h ::
	forall a . a ~ Label.Elem Text =>
	Optic.PartialIso' (Pos.Positioned (Accu.Accumulated Text)) (a (), (Paragraph, Bool)) (NodeH a)
layer_h = Optic.PartialIso render_from_node parse_into_node


type HTree a = (Tree (NodeH a))
type HTree' = HTree A

translate_page_name_char :: Base.Char -> Base.Char
translate_page_name_char c = if Base.isAlphaNum c then c else '-'

translate_page_name :: Text -> Text
translate_page_name = map translate_page_name_char

pages :: Tree (NodeH a) -> [NodeH a]
pages = toList >>> Base.filter Data.nodeIsSeparatePage

address_of_page :: NodeH A -> Text
address_of_page = Optic.to_list Data.texts_in_Node >>> Fold.fold >>> translate_page_name

add_address_to_page :: NodeH A -> (Text, NodeH A)
add_address_to_page n = (address_of_page n, n)

key_page_map :: Tree (NodeH A) -> Either (Base.String, [NodeH A]) (MapS.Map Base.Char (NodeH A))
key_page_map = pages >>> map add_address_to_page >>> Map.from_list_of_uniques

page_name_repetition_in_document :: Tree (NodeH A) -> Maybe (Pos.PositionedMb (Accu.Accumulated Text))
page_name_repetition_in_document tree =
	case key_page_map tree of
		Left repetition ->
			let message =
				Accu.single "Document is invalid. Page name ~ \"" <>
				Accu.single (fst repetition) <>
				Accu.single "\" is repeated"
				in Just (Pos.without_position message)
		Right _ -> Nothing

layer ::
	(a ~ Label.Elem Text) =>
	Optic.PartialIso' (Pos.PositionedMb (Accu.Accumulated Text)) (InputTree a) (Tree (NodeH a))
layer = 
	Category2.empty
	>**>^ Optic.lift_iso layer_MetaName
	>**>^ Optic.piso_convert_error Pos.without_position core_layer
	>**>^ Optic.piso_convert_error Pos.maybefy_positioned (Optic.lift_piso layer_h)
