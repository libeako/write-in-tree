module WriteInTree.Document.Core.Serial.RichTextTree.Label.Lower
(
	IntermediateTree, IntermediateBranchTree, render_trunk,
	render_labels_into_siblings, parse_labels_from_siblings,
	layer,
)
where

import Control.Monad ((>=>))
import Data.Default.Class
import Data.Functor (($>))
import Data.Tree (Tree)
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import Prelude (Bounded, Enum)

import qualified Data.Either as Base
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Maybe as Base
import qualified Data.Tree as Tree
import qualified Fana.Data.Key.Traversable as TravKey
import qualified Fana.Data.Tree.Uniform as FanaTree
import qualified Fana.Data.Tree.Discriminating as DTree
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified Technical.TextTree.Data as Tt
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.MetaStructure as Ms
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.TextStructure as Ts
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Path as Path
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Intermediate as Intermediate


type Char = Base.Char
type Text = [Char]
type ElemP = Path.ElemHP
type Source = ElemP ()
type ElemPT = ElemP Text

data MetaName = MnId | MnClass
	
	deriving (Bounded, Enum)

meta_name_to_text :: MetaName -> Text
meta_name_to_text = 
	\case
		MnId -> "id"
		MnClass -> "class"

type ElemStructured = Path.ElemHP (Either MetaName Ts.Content')

layer_in_node :: 
	Optic.PartialIso' (Pos.Positioned Ts.TextStructureError) (Tree Path.ElemHPT) (Tree ElemStructured)
layer_in_node = Ms.layer_1 meta_name_to_text

show_error_at :: (Path.ElemHP e) -> Accu.Accumulated Text -> Accu.Accumulated Text
show_error_at position description = Fana.show (Pos.Positioned (Pos.get_position position) description)

parse_id :: Tree ElemStructured -> Either (Accu.Accumulated Text) Intermediate.IdT
parse_id (Tree.Node trunk children) = 
	case children of
		[child] -> 
			let
				child_node = Tree.rootLabel child
				in case Tt.elemValue (Path.inElemHPCore child_node) of
					Right (Right text) -> 
						let
							intermediate :: Intermediate.IdT
							intermediate = Intermediate.Id
								{ Intermediate.source_of_id_trunk = trunk $> ()
								, Intermediate.source_of_id_value = child_node $> ()
								, Intermediate.valueId = text
								}
							in Right intermediate
					_ -> 
						let 
							description = "the node holding the identifier value must be regular text, not a meta node"
							in Left (show_error_at child_node description)
		_ -> Left (show_error_at trunk "number of children of an identifier node must be 1")

render_id :: Intermediate.IdT -> Tree ElemStructured
render_id x = 
	let
		trunk :: ElemStructured
		trunk = Intermediate.source_of_id_trunk x $> Left MnId
		child :: ElemStructured
		child = Intermediate.source_of_id_value x $> Right (Right (Intermediate.valueId x))
		in Tree.Node trunk [Tree.Node child []]

parse_class :: Tree ElemStructured -> Either (Accu.Accumulated Text) Intermediate.Class
parse_class (Tree.Node trunk _) = 
	case Tt.elemValue (Path.inElemHPCore trunk) of
		Right (Right text) -> Right (Intermediate.Class (trunk $> ()) text)
		_ -> 
			let
				description = "the node holding a class name must be regular text, not a meta node"
				in Left (show_error_at trunk description)

render_class :: (Text, ElemP ()) -> Tree ElemStructured
render_class (name, source) = Tree.Node (source $> Right (Right name)) []

parse_classes :: Tree ElemStructured -> Either (Accu.Accumulated Text) Intermediate.Classes
parse_classes (Tree.Node trunk children) = 
	map (Intermediate.Classes (Just (trunk $> ()))) (traverse parse_class children >>= Intermediate.index_classes)

render_classes' :: Intermediate.Classes -> Tree ElemStructured
render_classes' cs = let
	new_trunk :: ElemStructured
	new_trunk = let
		content = Left MnClass
		in case Intermediate.source_of_classes_trunk cs of
			Nothing -> def @(Path.ElemHP ()) $> content
			Just s -> s $> content
	in Tree.Node new_trunk (map render_class (TravKey.key_value_pairs (Intermediate.classes cs)))

render_classes :: Intermediate.Classes -> Maybe (Tree ElemStructured)
render_classes cs = let
	preliminary = render_classes' cs
	in if List.null (Tree.subForest preliminary) then Nothing else Just preliminary

type IntermediateTree = DTree.Tree [] Intermediate.Any (ElemP Ts.Content') ()
type IntermediateBranchTree = (ElemP Ts.Content', [IntermediateTree])

parse_any :: Tree ElemStructured -> Either (Accu.Accumulated Text) IntermediateTree
parse_any (tree@(Tree.Node trunk children)) = 
	let
		node_specific :: 
			Either (Accu.Accumulated Text) 
				(DTree.Discrimination [] Intermediate.Any (ElemP Ts.Content') IntermediateTree)
		node_specific = 
			case Tt.elemValue (Path.inElemHPCore trunk) of
				Left mn -> 
					case mn of
						MnId -> map (Intermediate.IntermId >>> DTree.Leaf) (parse_id tree)
						MnClass -> map (Intermediate.IntermClass >>> DTree.Leaf) (parse_classes tree)
				Right text -> map (DTree.Joint (trunk $> text)) (traverse parse_any children)
		in map (FanaTree.assemble ()) node_specific

parse_trunk :: IntermediateTree -> Either (Accu.Accumulated Text) IntermediateBranchTree
parse_trunk = 
	FanaTree.children >>> 
	\ case
		DTree.Leaf _ -> Left "the whole tree is a label which is invalid"
		DTree.Joint trunk children -> Right (trunk, children)

render_trunk :: IntermediateBranchTree -> IntermediateTree
render_trunk = uncurry DTree.Joint >>> FanaTree.assemble ()

layer_trunk :: Optic.PartialIso' (Accu.Accumulated Text) IntermediateTree IntermediateBranchTree
layer_trunk = Optic.PartialIso render_trunk parse_trunk

parse_all_any :: Tree ElemStructured -> Either (Accu.Accumulated Text) IntermediateBranchTree
parse_all_any = parse_any >=> parse_trunk

render_any_branch :: IntermediateBranchTree -> Tree ElemStructured
render_any_branch (elem, children) = Tree.Node (map Right elem) (Base.catMaybes (map render_any children))

render_any :: IntermediateTree -> Maybe (Tree ElemStructured)
render_any tree = 
	case FanaTree.children tree of
		DTree.Leaf inter -> 
			case inter of
				Intermediate.IntermId inter_id -> Just (render_id inter_id)
				Intermediate.IntermClass inter_class -> render_classes inter_class
		DTree.Joint elem children -> Just (render_any_branch (elem, children))

render_all_any :: IntermediateBranchTree -> Tree ElemStructured
render_all_any = render_any_branch

layer_any :: Optic.PartialIso' (Accu.Accumulated Text) (Tree ElemStructured) IntermediateBranchTree
layer_any = Optic.PartialIso render_all_any parse_all_any

layer :: Optic.PartialIso' (Accu.Accumulated Text) (Tree ElemPT) IntermediateBranchTree
layer = (Optic.piso_convert_error Fana.show layer_in_node) >**> layer_any


sort_intermediate_nodes :: [Intermediate.Any] -> ([Intermediate.IdT], [Intermediate.Classes])
sort_intermediate_nodes = 
	let
		to_either :: Intermediate.Any -> Either Intermediate.IdT Intermediate.Classes
		to_either = 
			\case 
				Intermediate.IntermId x -> Left x
				Intermediate.IntermClass x -> Right x
		in map to_either >>> Base.partitionEithers

parse_maybe_from_list :: [e] -> Either (Accu.Accumulated Text) (Maybe e)
parse_maybe_from_list = 
	\case
		[] -> Right Nothing
		[single] -> Right (Just single)
		_ -> Left "multiple labeling nodes with the same type is not valid"

parse_labels_from_sorted :: 
	([Intermediate.IdT], [Intermediate.Classes]) -> 
	Either (Accu.Accumulated Text) Intermediate.LabelsT
parse_labels_from_sorted (identifiers, classes') = 
	liftA2 Intermediate.Labels (parse_maybe_from_list identifiers) (parse_maybe_from_list classes')

parse_labels_from_siblings :: [Intermediate.Any] -> Either (Accu.Accumulated Text) Intermediate.LabelsT
parse_labels_from_siblings = sort_intermediate_nodes >>> parse_labels_from_sorted

render_labels_into_siblings :: Intermediate.LabelsT -> [Intermediate.Any]
render_labels_into_siblings labels = 
	let
		identifiers = Fold.toList (map Intermediate.IntermId (Intermediate.id_of_Labels labels))
		classes' = Fold.toList (map Intermediate.IntermClass (Intermediate.classes_of_Labels labels))
		in Fold.concat [identifiers, classes']
