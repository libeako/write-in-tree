module WriteInTree.Document.Core.Serial.RichTextTree.Label.Serialize
(
	Structure.add_new_classes_to_Labels,
	Configuration,
	layer,
	ofElem_pos,
	ofElem_class_values,
	inElem_labels,
	ofElem_classes,
	elem_has_class,
	Elem (..),
)
where

import Control.Monad ((>=>))
import Data.Default.Class
import Data.Functor (($>))
import Data.Traversable (sequence)
import Data.Tree (Tree (..), Forest)
import Fana.Data.HasSingle (HasSingle)
import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Elem
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..))
import WriteInTree.Document.Core.Serial.RichTextTree.Label.TextSplit (Configuration)
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (Positioned), get_position)

import qualified Control.Monad.State.Lazy as Base
import qualified Data.Bifunctor as BiFr
import qualified Data.Either as Base
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Maybe as Base
import qualified Data.Tree as Tree
import qualified Fana.Data.CollectionWithEmpty as Fana
import qualified Fana.Data.Function as Fn
import qualified Fana.Data.HasSingle as HasSingle
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.Key.Map.Interface as MapI
import qualified Fana.Data.Key.Map.KeyIsString as StringyMap
import qualified Fana.Data.Key.Traversable as TravKey
import qualified Fana.Data.List as List
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNodeTextStructure as Mtt
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.InlineClassCoding as Inline
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure as Structure
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos
import qualified Technical.TextTree.Data as Tt


type Char = Base.Char
type Text = [Char]
type ElemLR = Tt.Elem
type ElemP = Positioned
type ElemPT = ElemP Text
type ElemT = Elem Text
type Classes = Structure.ClassesMap

data IdRepetitionSearchInput =
	IdRepetitionSearchInput
	{ irsiGetter :: forall e . Elem e -> Maybe Text
	, irsiName :: Text
	}

id_repetition_search_input__machine :: IdRepetitionSearchInput
id_repetition_search_input__machine = IdRepetitionSearchInput ofElem_address "machine"

id_repetition_search_inputs :: [IdRepetitionSearchInput]
id_repetition_search_inputs =
	[
		id_repetition_search_input__machine
	]

any_repetition_in_id :: (ElemT -> Maybe Text) -> Tree ElemT -> [ElemT]
any_repetition_in_id id_getter = 
	let
		select :: [[ElemT]] -> [ElemT]
		select = 
			\ case
				[] -> []
				first : _ -> first
		in 
			id
			>>> Fold.toList 
			>>> map (liftA2 (,) id id_getter >>> Base.sequenceA) 
			>>> Base.catMaybes
			>>> map Pair.swap
			>>> MapI.from_list_lists @(StringyMap.Map Char)
			>>> Fold.toList 
			>>> List.filter (List.take 2 >>> List.length >>> (Base.> 1)) >>> select 
	

check_uniquness_of_id :: IdRepetitionSearchInput -> Tree ElemT -> Maybe (Accu.Accumulated Text)
check_uniquness_of_id id_type tree = 
	case any_repetition_in_id (irsiGetter id_type) tree of
		[] -> Nothing
		list -> 
			let
				per_line :: Accu.Accumulated Text -> Accu.Accumulated Text
				per_line content = "--- " <> content <> "\n"
				node_writer :: ElemT -> Accu.Accumulated Text
				node_writer = ofElem_core >>> get_position >>> Pos.show_position >>> per_line
				message :: Accu.Accumulated Text
				message =
					let
						text_intro :: Accu.Accumulated Text
						text_intro =
							Fold.fold
								(
									map Accu.single
										["the following nodes share a same identifier for ", irsiName id_type, " :\n"]
								)
						in Fold.foldl' (<>) text_intro (map node_writer list)
				in Just message

check_uniquness_of_ids :: Tree ElemT -> Maybe (Accu.Accumulated Text)
check_uniquness_of_ids =
	let
		uniquness_checks :: [Tree ElemT -> Maybe (Accu.Accumulated Text)]
		uniquness_checks = map check_uniquness_of_id id_repetition_search_inputs
		extract_single :: [Maybe e] -> Maybe e
		extract_single = Base.catMaybes >>> List.first
		in sequence uniquness_checks >>> extract_single


meta_name_address :: Text
meta_name_address = "address"

meta_name_class :: Text
meta_name_class = "class"

wrap_default :: forall a e . (Functor a, Default (a e)) => e -> a e
wrap_default = ((def :: a e) $>)

type Wrap a e = e -> a e

render_address_tree :: PageAddress -> Tree Text
render_address_tree address =
	Node (Mtt.render_exceptional meta_name_address) [Node (unwrapPageAddress address) []]

parse_address :: [Tree Text] -> Either (Accu.Accumulated Text) PageAddress
parse_address =
	\case
		[Node address []] -> Right (PageAddress address)
		_ -> Left (Accu.single "wrong format of page address text value [must be a single tree node]")

parse_address_tree_as_exceptional ::
	HasSingle a => Tree (a Text) -> Either (Either (Accu.Accumulated Text) PageAddress) (Tree (a Text))
parse_address_tree_as_exceptional tree@(Node trunk children) =
	if HasSingle.elem trunk == (Mtt.render_exceptional meta_name_address)
		then Left (parse_address ((map >>> map) HasSingle.elem children))
		else Right tree

render_address_into_siblings :: Maybe PageAddress -> Fn.Endo (Forest Text)
render_address_into_siblings =
	\case
		Nothing -> id
		Just address -> (render_address_tree address :)

render_class_tree :: [Text] -> Tree Text
render_class_tree classes =
	Node (Mtt.render_exceptional meta_name_class) (map (flip Node []) classes)

parse_classes_tree_as_exceptional ::
	forall a .
	HasSingle a =>
	Tree (a Text) -> Either (Either (Accu.Accumulated Text) [Text]) (Tree (a Text))
parse_classes_tree_as_exceptional tree@(Node trunk children) =
	if HasSingle.elem trunk == (Mtt.render_exceptional meta_name_class)
		then Left (Right (map (rootLabel >>> HasSingle.elem) children))
		else Right tree

render_classes_into_siblings :: Classes -> Fn.Endo (Forest Text)
render_classes_into_siblings classes =
	let
		class_list :: [Text]
		class_list = TravKey.keys classes
		in
			case class_list of
				[] -> id
				_ -> (render_class_tree class_list :)

parse_address_from_siblings ::
	forall a .
	HasSingle a =>
	Base.StateT (Forest (a Text)) (Either (Accu.Accumulated Text)) (Maybe PageAddress)
parse_address_from_siblings =
	let
		raw :: Forest (a Text) -> Either (Accu.Accumulated Text) (Maybe PageAddress, Forest (a Text))
		raw siblings =
			let
				(address_results, normal_children) =
					Base.partitionEithers (map parse_address_tree_as_exceptional siblings)
				in
					case address_results of
						[] -> Right (Nothing, normal_children)
						(first_address_result : _) ->
							map (Just >>> Pair.before normal_children) first_address_result
		in Base.StateT raw

parse_classes_from_siblings ::
	forall a .
	HasSingle a =>
	Base.StateT (Forest (a Text)) (Either (Accu.Accumulated Text)) Classes
parse_classes_from_siblings =
	let
		raw :: Forest (a Text) -> Either (Accu.Accumulated Text) (Classes, Forest (a Text))
		raw siblings =
			let
				classes_results :: [Either (Accu.Accumulated Text) [Text]]
				(classes_results, normal_children) =
					Base.partitionEithers (map parse_classes_tree_as_exceptional siblings)
				classes_result :: Either (Accu.Accumulated Text) [[Text]]
				classes_result = Base.sequence classes_results
				merge_classes :: [[Text]] -> Either (Accu.Accumulated Text) Classes
				merge_classes = Fold.fold >>> Structure.index_classes
				in map (Pair.before normal_children) (classes_result >>= merge_classes)
		in Base.StateT raw

render_all_into_siblings :: (Maybe PageAddress, Classes) -> Fn.Endo (Forest Text)
render_all_into_siblings (address, classes) =
	render_classes_into_siblings classes >>>
	render_address_into_siblings address

parse_all_from_siblings ::
	forall a .
	HasSingle a =>
	Base.StateT (Forest (a Text)) (Either (Accu.Accumulated Text)) (Maybe PageAddress, Classes)
parse_all_from_siblings =
	liftA2 (,) parse_address_from_siblings parse_classes_from_siblings

render_tree :: Tree ElemT -> Tree ElemLR
render_tree (Node trunk children) =
	let
		labels = ofElem_labels trunk
		address :: Maybe PageAddress
		address = Structure.address_of_Labels labels
		classes :: Classes
		classes = maybe Fana.empty_coll id (Structure.classes_of_Labels labels)
		in
			Node (HasSingle.elem trunk)
				(render_all_into_siblings (address, classes) (map render_tree children))

parse_tree_r :: Tree ElemPT -> Either (Pos.PositionedMb (Accu.Accumulated Text)) (Tree ElemT)
parse_tree_r (Node trunk all_children) =
	let
		current_parse_result ::
			Either (Pos.PositionedMb (Accu.Accumulated Text))
				((Maybe PageAddress, Classes), [Tree ElemPT])
		current_parse_result =
			BiFr.first (Pos.position_error_mb trunk)
			(Base.runStateT parse_all_from_siblings all_children)
		continue_parse_result ::
			((Maybe PageAddress, Classes), [Tree ElemPT]) ->
			Either (Pos.PositionedMb (Accu.Accumulated Text)) (Tree ElemT)
		continue_parse_result ((page_address, classes), normal_children) =
			let
				position = Pos.get_position trunk
				classes_mb = if Fana.is_coll_empty classes then Nothing else (Just classes)
				labels = Structure.Labels page_address classes_mb
				in
					map (Node (Elem labels (Positioned position (HasSingle.elem trunk))))
						(traverse parse_tree_r normal_children)
		in current_parse_result >>= continue_parse_result

parse_tree :: Tree ElemPT -> Either (Pos.PositionedMb (Accu.Accumulated Text)) (Tree ElemT)
parse_tree =
	let
		from_tree tree =
			BiFr.first (Pos.position_error_mb (ofElem_core (Tree.rootLabel tree)))
			(
				case check_uniquness_of_ids tree of
					Just e -> Left e
					Nothing -> Right tree
			)			
		in parse_tree_r >=> from_tree

layer_new_simple ::
	Optic.PartialIso (Pos.PositionedMb (Accu.Accumulated Text))
		(Tree ElemLR) (Tree ElemPT) (Tree ElemT) (Tree ElemT)
layer_new_simple = Optic.PartialIso render_tree parse_tree


layer :: Configuration ->
	Optic.PartialIso (Pos.PositionedMb (Accu.Accumulated Text))
		(Tree ElemLR) (Tree ElemPT) (Tree ElemT) (Tree ElemT)
layer config =
	Cat2.identity
	>**> layer_new_simple
	>**> convert_from_describing_class_4 (Optic.lift_iso (Inline.layer config))
