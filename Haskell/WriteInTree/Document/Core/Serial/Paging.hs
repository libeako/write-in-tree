module WriteInTree.Document.Core.Serial.Paging
(
	CoreHTree, CoreHTree', HTree, HTree',
	layer,
)
where

import Data.Tree (Tree)
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Fold
import qualified Data.Tree as Tree
import qualified Fana.Data.Either as Either
import qualified Fana.Data.Function as Fn
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.MetaName as Mn
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.MetaStructure as Ms
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.ClassPrefix as Class
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String

type Paragraph a = Data.Paragraph (a ()) (a ()) Text Text
type InputElem a = (a (), Either Text (Paragraph a))
type A = Label.Elem Text
type InputTree a = Tree (InputElem a)

data MetaNodeName = MnLinksTo deriving (Base.Enum, Base.Bounded)

-- | the text value of a linked-contents meta node name
text_linked_contents :: Text
text_linked_contents = "linked-contents"

render_MetaNodeName :: MetaNodeName -> Text
render_MetaNodeName = \case { MnLinksTo -> text_linked_contents }

layer_MetaName' :: Optic.PartialIso' (Accu.Accumulated Text) Text MetaNodeName
layer_MetaName' = let
	parse :: Text -> Either (Accu.Accumulated Text) MetaNodeName
	parse = id 
		>>> Optic.up (Mn.iso render_MetaNodeName)
		>>> Either.swap 
		>>> Bifunctor.first (const "unexpected meta node name")
	in Optic.PartialIso render_MetaNodeName parse

layer_MetaName :: (forall x . Pos.HasPosition (a x)) => Optic.PartialIso' (Pos.Positioned (Accu.Accumulated Text))
	(a (), Either Text (Paragraph a)) 
	(a (), Either MetaNodeName (Paragraph a))
layer_MetaName = Optic.piso_convert_error_with_input (fst >>> Pos.position_error) 
	(Optic.lift_piso (Ms.lift_layer_to_Left layer_MetaName'))

-- | text value of node class signalling the separate page status
text_page_class :: Text
text_page_class = Class.class_prefix <> "page"

store_separate_page_status :: Bool -> Fn.Endo (Label.Elem id e)
store_separate_page_status separate_page = if separate_page
	then Optic.fn_up Label.inElem_labels (Label.add_new_classes_to_Labels [text_page_class])
	else id

type CoreLTree a = Tree (a (), Either MetaNodeName (Paragraph a))
type CoreHTree a = Tree (a (), (Paragraph a, Bool))

core_render :: CoreHTree a -> CoreLTree a
core_render = (map >>> map) (fst >>> Right)

has_page_class :: Label.Elem id e -> Bool
has_page_class = Label.elem_has_class text_page_class

core_parse' :: forall a id . a ~ Label.Elem id =>
	Bool -> CoreLTree a -> [CoreHTree a]
core_parse' separate_page_as_inherited (Tree.Node (a, ei_paragraph) children) = let
	make_children :: Bool ->[CoreHTree a]
	make_children sp = Fold.concat (map (core_parse' sp) children)
	on_regular :: Paragraph a -> [CoreHTree a]
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

type NodeH a = Data.Node (a ()) (a ()) Text Text Text

parse_into_node :: 
	forall a . a ~ Label.Elem Text => 
	(a (), (Paragraph a, Bool)) -> Either (Pos.Positioned (Accu.Accumulated Text)) (NodeH a)
parse_into_node (a, (paragraph, is_separate_page)) = let
	make :: 
		Text ->
		Data.Node (a ()) (a ()) Text Text Text
	make id_a = Data.Node id_a a (a, paragraph) is_separate_page
	error_message :: Pos.Positioned (Accu.Accumulated Text)
	error_message = Pos.Positioned (Pos.get_position a) "node does not have an automatic identifier"
	in Base.maybe (Left error_message) Right
		(map make (Label.ofElem_auto_id a))

render_from_node :: NodeH a -> (a (), (Paragraph a, Bool))
render_from_node i = (fst (Data.nodeContent i), (snd (Data.nodeContent i), Data.nodeIsSeparatePage i))

layer_h :: 
	forall a . a ~ Label.Elem Text => 
	Optic.PartialIso' (Pos.Positioned (Accu.Accumulated Text)) (a (), (Paragraph a, Bool)) (NodeH a)
layer_h = Optic.PartialIso render_from_node parse_into_node


type HTree a = (Tree (NodeH a))
type HTree' = HTree A

layer :: 
	(forall x . Pos.HasPosition (a x)) => (a ~ Label.Elem Text) => 
	Optic.PartialIso' (Pos.PositionedMb (Accu.Accumulated Text)) (InputTree a) (Tree (NodeH a))
layer = Category2.empty 
	>**> Optic.piso_convert_error Pos.maybefy_positioned (Optic.lift_piso layer_MetaName)
	>**> Optic.piso_convert_error Pos.without_position core_layer
	>**> Optic.piso_convert_error Pos.maybefy_positioned (Optic.lift_piso layer_h)
