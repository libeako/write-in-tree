module WriteInTree.Document.Core.Serial.RichTextTree.Label.Serialize
(
	Structure.add_new_classes_to_Labels,
	Structure.inLabel_id_source_mb,
	Structure.id_of_Labels,
	Configuration,
	layer,
	fromElem_id_au_content,
	ofElem_pos,
	ofElem_class_values,
	ofElem_id_u_content,
	inElem_idu, inElem_labels,
	ofElem_classes,
	elem_has_class,
	Elem (..),
)
where

import Control.Monad ((>=>))
import Data.Default.Class
import Data.Functor (($>))
import Data.Tree (Tree (..), Forest)
import Fana.Data.HasSingle (HasSingle)
import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Elem
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..))
import WriteInTree.Document.Core.Serial.RichTextTree.Label.TextSplit (Configuration)

import qualified Control.Monad.State.Lazy as Base
import qualified Data.Either as Base
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Maybe as Base
import qualified Fana.Data.CollectionWithEmpty as Fana
import qualified Fana.Data.Function as Fn
import qualified Fana.Data.HasSingle as HasSingle
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.Key.Map.Interface as MapI
import qualified Fana.Data.Key.Map.KeyIsString as StringyMap
import qualified Fana.Data.Key.Traversable as TravKey
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNodeTextStructure as Mtt
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.InlineClassCoding as Inline
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure as Structure
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Path as Path
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos
import qualified Technical.TextTree.Data as Tt


type Char = Base.Char
type Text = [Char]
type ElemLRT = Path.ElemHRT
type ElemP = Path.ElemHP
type ElemPT = ElemP Text
type ElemTT = Elem Text Text
type Classes = Structure.ClassesMap

any_repetition_in_id_u :: Tree ElemTT -> [ElemTT]
any_repetition_in_id_u = 
	let
		select :: [[ElemTT]] -> [ElemTT]
		select = 
			\ case
				[] -> []
				first : _ -> first
		in 
			id
			>>> Fold.toList 
			>>> map (liftA2 (,) id ofElem_id_u_content >>> Base.sequenceA) 
			>>> Base.catMaybes
			>>> map Pair.swap
			>>> MapI.from_list_lists @(StringyMap.Map Char)
			>>> Fold.toList 
			>>> List.filter (List.take 2 >>> List.length >>> (Base.> 1)) >>> select 
	

check_uniquness_of_id_u :: Tree ElemTT -> Either (Accu.Accumulated Text) (Tree ElemTT)
check_uniquness_of_id_u tree = 
	case any_repetition_in_id_u tree of
		[] -> Right tree
		list -> 
			let
				per_line :: Accu.Accumulated Text -> Accu.Accumulated Text
				per_line content = "--- " <> content <> "\n"
				node_writer :: ElemTT -> Accu.Accumulated Text
				node_writer = ofElem_position >>> Pos.show_position >>> per_line
				message :: Accu.Accumulated Text
				message = 
					Fold.foldl' (<>) "the following nodes share a same user given identifier :\n"
						(map node_writer list)
				in Left message

meta_name_id :: Text
meta_name_id = "id"

meta_name_address :: Text
meta_name_address = "address"

meta_name_class :: Text
meta_name_class = "class"

wrap :: forall a e . (Functor a, Default (a e)) => e -> a e
wrap = ((def :: a e) $>)

render_id_tree :: Text -> Tree Text
render_id_tree identifier =
	Node (Mtt.render_exceptional meta_name_id) [Node identifier []]

render_address_tree :: PageAddress -> Tree Text
render_address_tree address =
	Node (Mtt.render_exceptional meta_name_address) [Node (unwrapPageAddress address) []]

parse_identifier :: [Tree Text] -> Either (Accu.Accumulated Text) Text
parse_identifier =
	\case
		[Node identifier []] -> Right identifier
		_ -> Left (Accu.single "wrong format of identifier text value [must be a single tree node]")

parse_address :: [Tree Text] -> Either (Accu.Accumulated Text) PageAddress
parse_address =
	\case
		[Node address []] -> Right (PageAddress address)
		_ -> Left (Accu.single "wrong format of page address text value [must be a single tree node]")

parse_id_tree_as_exceptional ::
	HasSingle a => Tree (a Text) -> Either (Either (Accu.Accumulated Text) Text) (Tree (a Text))
parse_id_tree_as_exceptional tree@(Node trunk children) =
	if HasSingle.elem trunk == (Mtt.render_exceptional meta_name_id)
		then Left (parse_identifier ((map >>> map) HasSingle.elem children))
		else Right tree

parse_address_tree_as_exceptional ::
	HasSingle a => Tree (a Text) -> Either (Either (Accu.Accumulated Text) PageAddress) (Tree (a Text))
parse_address_tree_as_exceptional tree@(Node trunk children) =
	if HasSingle.elem trunk == (Mtt.render_exceptional meta_name_address)
		then Left (parse_address ((map >>> map) HasSingle.elem children))
		else Right tree

render_id_into_siblings ::
	forall a .
	(Functor a, Default (a Text)) =>
	Maybe Text -> Fn.Endo (Forest (a Text))
render_id_into_siblings =
	\case
		Nothing -> id
		Just identifier -> (map wrap (render_id_tree identifier) :)

render_address_into_siblings ::
	forall a .
	(Functor a, Default (a Text)) =>
	Maybe PageAddress -> Fn.Endo (Forest (a Text))
render_address_into_siblings =
	\case
		Nothing -> id
		Just address -> (map wrap (render_address_tree address) :)

parse_id_from_siblings ::
	forall a .
	HasSingle a =>
	Base.StateT (Forest (a Text)) (Either (Accu.Accumulated Text)) (Maybe Text)
parse_id_from_siblings =
	let
		raw :: Forest (a Text) -> Either (Accu.Accumulated Text) (Maybe Text, Forest (a Text))
		raw siblings =
			let
				(identifier_results, normal_children) =
					Base.partitionEithers (map parse_id_tree_as_exceptional siblings)
				in
					case identifier_results of
						[] -> Right (Nothing, normal_children)
						(first_id_result : _) ->
							map (Just >>> Pair.before normal_children) first_id_result
		in Base.StateT raw

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

render_classes_into_siblings ::
	forall a .
	(Functor a, Default (a Text)) =>
	Classes -> Fn.Endo (Forest (a Text))
render_classes_into_siblings classes =
	let
		class_list :: [Text]
		class_list = TravKey.keys classes
		in
			case class_list of
				[] -> id
				_ -> (map wrap (render_class_tree class_list) :)

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

render_all_into_siblings ::
	forall a .
	(Functor a, Default (a Text)) =>
	(Maybe Text, Maybe PageAddress, Classes) -> Fn.Endo (Forest (a Text))
render_all_into_siblings (identifier_mb, address, classes) =
	render_classes_into_siblings classes >>>
	render_address_into_siblings address >>>
	render_id_into_siblings identifier_mb

parse_all_from_siblings ::
	forall a .
	HasSingle a =>
	Base.StateT (Forest (a Text)) (Either (Accu.Accumulated Text)) (Maybe Text, Maybe PageAddress, Classes)
parse_all_from_siblings =
	liftA3 (,,) parse_id_from_siblings parse_address_from_siblings parse_classes_from_siblings

render_tree :: Tree ElemTT -> Tree ElemLRT
render_tree (Node trunk children) =
	let
		labels = ofElem_labels trunk
		identifier :: Maybe Text
		identifier = Structure.id_of_Labels labels
		address :: Maybe PageAddress
		address = Structure.address_of_Labels labels
		classes :: Classes
		classes = maybe Fana.empty_coll id (Structure.classes_of_Labels labels)
		in
			Node (Tt.Elem (ofElem_auto_id trunk) (HasSingle.elem trunk))
				(render_all_into_siblings (identifier, address, classes) (map render_tree children))

parse_tree_r :: Tree ElemPT -> Either (Accu.Accumulated Text) (Tree ElemTT)
parse_tree_r (Node trunk all_children) =
	let
		current_parse_result :: Either (Accu.Accumulated Text) ((Maybe Text, Maybe PageAddress, Classes), [Tree ElemPT])
		current_parse_result = Base.runStateT parse_all_from_siblings all_children
		continue_parse_result ::
			((Maybe Text, Maybe PageAddress, Classes), [Tree ElemPT]) ->
			Either (Accu.Accumulated Text) (Tree ElemTT)
		continue_parse_result ((user_id, page_address, classes), normal_children) =
			let
				auto_id = Tt.elemId (Path.inElemHPCore trunk)
				position = Path.inElemHPPos trunk
				classes_mb = if Fana.is_coll_empty classes then Nothing else (Just classes)
				labels = Structure.Labels user_id page_address classes_mb
				in
					map (Node (Elem auto_id position labels (HasSingle.elem trunk)))
						(traverse parse_tree_r normal_children)
		in current_parse_result >>= continue_parse_result

parse_tree :: Tree ElemPT -> Either (Accu.Accumulated Text) (Tree ElemTT)
parse_tree = parse_tree_r >=> check_uniquness_of_id_u

layer_new_simple :: Optic.PartialIso (Accu.Accumulated Text) (Tree ElemLRT) (Tree ElemPT) (Tree ElemTT) (Tree ElemTT)
layer_new_simple = Optic.PartialIso render_tree parse_tree


layer :: Configuration -> Optic.PartialIso (Accu.Accumulated Text) (Tree ElemLRT) (Tree ElemPT) (Tree ElemTT) (Tree ElemTT)
layer config =
	Cat2.identity
	>**> layer_new_simple
	>**> convert_from_describing_class_4 (Optic.lift_iso (Inline.layer config))