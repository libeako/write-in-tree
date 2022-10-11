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

import Data.Tree (Tree)
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

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Either as Base
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Maybe as Base
import qualified Data.Tree as Tree
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.Key.Map.Interface as MapI
import qualified Fana.Data.Key.Map.KeyIsString as StringyMap
import qualified Fana.Data.Tree.Discriminating as DTree
import qualified Fana.Data.Tree.Uniform as FanaTree
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.InlineClassCoding as Inline
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Intermediate as Intermediate
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Lower as Lower
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Path as Path
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Char = Base.Char
type Text = [Char]
type ElemLR e = Path.ElemHR e
type ElemLRT = Path.ElemHRT
type ElemP = Path.ElemHP
type Source = ElemP ()
type ElemPT = ElemP Text
type ElemTT = Elem Text Text

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

layer :: Configuration -> Optic.PartialIso (Accu.Accumulated Text) (Tree ElemLRT) (Tree ElemPT) Data Data
layer config =
	Cat2.identity
	>**> Lower.layer
	>**> layer_up_from_intermediate
	>**> convert_from_describing_class_4 (Optic.lift_iso (Inline.layer config))
