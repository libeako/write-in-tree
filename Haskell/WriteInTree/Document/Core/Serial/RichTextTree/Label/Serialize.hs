module WriteInTree.Document.Core.Serial.RichTextTree.Label.Serialize
(
	layer,
	LabeledPositioned (..),
)
where

import Control.Monad ((>=>))
import Data.Traversable (sequence)
import Data.Tree (Tree (..), Forest)
import Fana.Data.HasSingle (HasSingle)
import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..), address_of_Labels, Labels)
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (Positioned), get_position)

import qualified Control.Monad.State.Lazy as Base
import qualified Data.Bifunctor as BiFr
import qualified Data.Either as Base
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Maybe as Base
import qualified Data.Tree as Tree
import qualified Fana.Data.Function as Fn
import qualified Fana.Data.HasSingle as HasSingle
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.Key.Map.Interface as MapI
import qualified Fana.Data.Key.Map.KeyIsString as StringyMap
import qualified Fana.Data.List as List
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNodeTextStructure as Mtt
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure as Structure
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Char = Base.Char
type Text = [Char]
type ElemLR = Text
type ElemP = Positioned
type ElemPT = ElemP Text
type LabeledPositioned e = (Labels, Positioned e)
type ElemT = LabeledPositioned Text

data IdRepetitionSearchInput =
	IdRepetitionSearchInput
	{ irsiGetter :: forall e . LabeledPositioned e -> Maybe Text
	, irsiName :: Text
	}

id_repetition_search_input__machine :: IdRepetitionSearchInput
id_repetition_search_input__machine = 
	IdRepetitionSearchInput (fst >>> address_of_Labels >>> map unwrapPageAddress) "machine"

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
				node_writer = snd >>> get_position >>> Pos.show_position >>> per_line
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
render_address_into_siblings = const id

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

render_tree :: Tree ElemT -> Tree ElemLR
render_tree (Node trunk children) =
	let
		labels = fst trunk
		address :: Maybe PageAddress
		address = Structure.address_of_Labels labels
		in
			Node ((HasSingle.elem >>> HasSingle.elem) trunk)
				(render_address_into_siblings address (map render_tree children))

parse_tree_r :: Tree ElemPT -> Either (Pos.PositionedMb (Accu.Accumulated Text)) (Tree ElemT)
parse_tree_r (Node trunk all_children) =
	let
		current_parse_result ::
			Either (Pos.PositionedMb (Accu.Accumulated Text))
				(Maybe PageAddress, [Tree ElemPT])
		current_parse_result =
			BiFr.first (Pos.position_error_mb trunk)
			(Base.runStateT parse_address_from_siblings all_children)
		continue_parse_result ::
			(Maybe PageAddress, [Tree ElemPT]) ->
			Either (Pos.PositionedMb (Accu.Accumulated Text)) (Tree ElemT)
		continue_parse_result (page_address, normal_children) =
			let
				position = Pos.get_position trunk
				labels = Structure.Labels page_address
				in
					map (Node (labels, (Positioned position (HasSingle.elem trunk))))
						(traverse parse_tree_r normal_children)
		in current_parse_result >>= continue_parse_result

parse_tree :: Tree ElemPT -> Either (Pos.PositionedMb (Accu.Accumulated Text)) (Tree ElemT)
parse_tree =
	let
		from_tree tree =
			BiFr.first (Pos.position_error_mb (snd (Tree.rootLabel tree)))
			(
				case check_uniquness_of_ids tree of
					Just e -> Left e
					Nothing -> Right tree
			)			
		in parse_tree_r >=> from_tree

layer ::
	Optic.PartialIso (Pos.PositionedMb (Accu.Accumulated Text))
		(Forest ElemLR) (Forest ElemPT) (Forest ElemT) (Forest ElemT)
layer = Optic.lift_piso (Optic.PartialIso render_tree parse_tree)
