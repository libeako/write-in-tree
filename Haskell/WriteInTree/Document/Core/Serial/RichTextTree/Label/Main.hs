module WriteInTree.Document.Core.Serial.RichTextTree.Label.Main
(
	Intermediate.add_new_classes_to_Labels,
	Intermediate.inLabel_id_source_mb,
	Intermediate.id_of_Labels,
	Configuration,
	layer,
	fromElem_id_au_content,
	ofElem_pos,
	ofElem_class_values,
	ofElem_id_u_content,
	inElem_idu, inElem_labels,
	ofElem_classes,
	elem_has_class,
	Elem (..), ElemT, Data,
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
import WriteInTree.Document.Core.Serial.RichTextTree.Label.TextSplit (Configuration)
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Lower 
	(
		IntermediateTreeP, IntermediateTreeR, IntermediateBranchTreeP, IntermediateBranchTreeR,
		render_labels_into_siblings,
		parse_labels_from_siblings
	)

import qualified Control.Monad.State.Lazy as Base
import qualified Data.Bifunctor as Bifunctor
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
import qualified Fana.Data.Tree.Discriminating as DTree
import qualified Fana.Data.Tree.Uniform as FanaTree
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.TextStructure as Mtt
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Elem as Elem
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.InlineClassCoding as Inline
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Intermediate as Intermediate
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Lower as Lower
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Path as Path
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos
import qualified Technical.TextTree.Data as Tt


type Char = Base.Char
type Text = [Char]
type ElemLR e = Path.ElemHR e
type ElemLRT = Path.ElemHRT
type ElemP = Path.ElemHP
type Source = ElemP ()
type ElemPT = ElemP Text
type ElemTT = Elem Text Text
type Classes = Intermediate.ClassesMap

parse_from_intermediate_branch :: 
	(ElemP Text, [IntermediateTreeP]) -> 
	Either (Accu.Accumulated Text) (Tree ElemTT)
parse_from_intermediate_branch (elem, children) =
	let
		child_to_Either :: IntermediateTreeP -> Either Intermediate.Any (ElemP Text, [IntermediateTreeP])
		child_to_Either tree =
			case FanaTree.children tree of
				DTree.Leaf l -> Left l
				DTree.Joint b c -> Right (b, c)
		(label_children, regular_children) = Base.partitionEithers (map child_to_Either children)
		add_position_to_error :: Accu.Accumulated Text -> Accu.Accumulated Text
		add_position_to_error = Pos.Positioned (Pos.get_position elem) >>> Fana.show
		in
			do
				labels <- Bifunctor.first add_position_to_error (parse_labels_from_siblings label_children)
				new_children <- traverse parse_from_intermediate_branch regular_children
				pure (Tree.Node (elem_pd labels elem) new_children)

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

parse_from_intermediate :: IntermediateBranchTreeP -> Either (Accu.Accumulated Text) (Tree ElemTT)
parse_from_intermediate (b, c) = parse_from_intermediate_branch (b, c) >>= check_uniquness_of_id_u

render_into_intermediate :: Tree ElemTT -> IntermediateBranchTreeR
render_into_intermediate (Tree.Node elem children) = 
	let
		regular_new_children, labeling_new_children, new_children :: [IntermediateTreeR]
		regular_new_children = map (render_into_intermediate >>> Lower.render_trunk) children
		labeling_new_children = 
			(render_labels_into_siblings >>> (map (DTree.leaf ()))) 
				(ofElem_labels elem)
		new_children = Fold.concat [regular_new_children, labeling_new_children]
		in (Path.inElemHPCore (elem_dp elem), new_children)

layer_up_from_intermediate ::
	Optic.PartialIso (Accu.Accumulated Text)
		IntermediateBranchTreeR IntermediateBranchTreeP
		(Tree ElemTT) (Tree ElemTT)
layer_up_from_intermediate = Optic.PartialIso render_into_intermediate parse_from_intermediate


type Data = Tree ElemTT


-- new, hopefully simpler implementation:

wrap :: forall a e . (Functor a, Default (a e)) => e -> a e
wrap = ((def :: a e) $>)

render_id_tree :: Text -> Tree Text
render_id_tree identifier =
	Node (Mtt.render_exceptional Lower.meta_name_id) [Node identifier []]

parse_identifier :: [Tree Text] -> Either (Accu.Accumulated Text) Text
parse_identifier =
	\case
		[Node identifier []] -> Right identifier
		_ -> Left (Accu.single "wrong format of identifier text value [must be a single tree node]")

parse_id_tree_as_exceptional ::
	HasSingle a => Tree (a Text) -> Either (Either (Accu.Accumulated Text) Text) (Tree (a Text))
parse_id_tree_as_exceptional tree@(Node trunk children) =
	if HasSingle.elem trunk == (Mtt.render_exceptional Lower.meta_name_id)
		then Left (parse_identifier ((map >>> map) HasSingle.elem children))
		else Right tree

render_id_into_siblings ::
	forall a .
	(Functor a, Default (a Text)) =>
	Maybe Text -> Fn.Endo (Forest (a Text))
render_id_into_siblings =
	\case
		Nothing -> id
		Just identifier -> (map wrap (render_id_tree identifier) :)

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
	Node (Mtt.render_exceptional Lower.meta_name_class) (map (flip Node []) classes)

parse_classes_tree_as_exceptional ::
	forall a .
	HasSingle a =>
	Tree (a Text) -> Either (Either (Accu.Accumulated Text) [Text]) (Tree (a Text))
parse_classes_tree_as_exceptional tree@(Node trunk children) =
	if HasSingle.elem trunk == (Mtt.render_exceptional Lower.meta_name_class)
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

parse_classes_from_siblings ::
	forall a .
	HasSingle a =>
	Base.StateT (Forest (a Text)) (Either (Accu.Accumulated Text)) Classes
parse_classes_from_siblings =
	let
		raw :: Forest (a Text) -> Either (Accu.Accumulated Text) (Classes, Forest (a Text))
		raw siblings =
			let
				(classes_results, normal_children) =
					Base.partitionEithers (map parse_classes_tree_as_exceptional siblings)
				merge_classes = Fold.fold >>> map (Pair.before ()) >>> MapI.from_list
				in
					map (merge_classes >>> Pair.before normal_children)
						(Base.sequence classes_results)
		in Base.StateT raw

render_all_into_siblings ::
	forall a .
	(Functor a, Default (a Text)) =>
	(Maybe Text, Classes) -> Fn.Endo (Forest (a Text))
render_all_into_siblings (identifier_mb, classes) =
	render_classes_into_siblings classes >>>
	render_id_into_siblings identifier_mb

parse_all_from_siblings ::
	forall a .
	HasSingle a =>
	Base.StateT (Forest (a Text)) (Either (Accu.Accumulated Text)) (Maybe Text, Classes)
parse_all_from_siblings =
	liftA2 (,) parse_id_from_siblings parse_classes_from_siblings

render_tree :: Tree ElemTT -> Tree ElemLRT
render_tree (Node trunk children) =
	let
		labels = ofElem_labels trunk
		identifier :: Maybe Text
		identifier = Elem.id_of_Labels labels
		classes :: Classes
		classes = maybe Fana.empty_coll id (Intermediate.classes_of_Labels labels)
		in
			Node (Tt.Elem (ofElem_auto_id trunk) (HasSingle.elem trunk))
				(render_all_into_siblings (identifier, classes) (map render_tree children))

parse_tree_r :: Tree ElemPT -> Either (Accu.Accumulated Text) (Tree ElemTT)
parse_tree_r (Node trunk all_children) =
	let
		current_parse_result :: Either (Accu.Accumulated Text) ((Maybe Text, Classes), [Tree ElemPT])
		current_parse_result = Base.runStateT parse_all_from_siblings all_children
		continue_parse_result ::
			((Maybe Text, Classes), [Tree ElemPT]) ->
			Either (Accu.Accumulated Text) (Tree ElemTT)
		continue_parse_result ((user_id, classes), normal_children) =
			let
				auto_id = Tt.elemId (Path.inElemHPCore trunk)
				position = Path.inElemHPPos trunk
				classes_mb = if Fana.is_coll_empty classes then Nothing else (Just classes)
				labels = Intermediate.Labels user_id classes_mb
				in
					map (Node (Elem auto_id position labels (HasSingle.elem trunk)))
						(traverse parse_tree_r normal_children)
		in current_parse_result >>= continue_parse_result

parse_tree :: Tree ElemPT -> Either (Accu.Accumulated Text) (Tree ElemTT)
parse_tree = parse_tree_r >=> check_uniquness_of_id_u

layer_new_simple :: Optic.PartialIso (Accu.Accumulated Text) (Tree ElemLRT) (Tree ElemPT) Data Data
layer_new_simple = Optic.PartialIso render_tree parse_tree


layer :: Configuration -> Optic.PartialIso (Accu.Accumulated Text) (Tree ElemLRT) (Tree ElemPT) Data Data
layer config =
	Cat2.identity
	-- ~ >**> Lower.layer
	-- ~ >**> layer_up_from_intermediate
	>**> layer_new_simple
	>**> convert_from_describing_class_4 (Optic.lift_iso (Inline.layer config))
