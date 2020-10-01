module WriteInTree.Document.Core.Serial.InlineContent
(
	Elem,
	Whole, layer
)
where

import Data.Functor ((<$), ($>))
import Data.Tree (Tree (..))
import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import Prelude (($))

import qualified Data.Tree as Tree
import qualified Fana.Data.HasSingle as HasSingle
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.MetaStructure as Ms
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.TextStructure as Ts
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String


data MetaNodeName = MnImage	| MnSpace
	deriving (Base.Enum, Base.Bounded)

render_MetaNodeName :: MetaNodeName -> Text
render_MetaNodeName = \case
	MnImage -> "image"
	MnSpace -> "space"

type ElemIntermediate = Label.Elem Text (Either MetaNodeName Ts.Content')
type Elem = Label.Elem Text (Data.InlineVisual (Label.Elem Text ()) Ts.Content')
type WholeIntermediate = Tree ElemIntermediate
type Whole = Tree Elem

layer_in_node :: Optic.Iso' (Tree (Label.Elem Text Ts.Content')) WholeIntermediate
layer_in_node = Ms.layer_not_1 render_MetaNodeName 


type ParseError = Pos.Positioned (Accu.Accumulated Text)
type Parsed = Either ParseError


parse_image :: WholeIntermediate -> Parsed Whole
parse_image (Tree.Node trunk children) = let
	error :: ElemIntermediate -> Text -> Parsed Whole
	error position description = 
		Left $ Pos.Positioned (Pos.get_position position) (Accu.single description)
	in case children of
		[] -> error trunk "picture node misses child, for file path"
		[single_child] -> let
			elem_in_child :: ElemIntermediate
			elem_in_child = Tree.rootLabel single_child
			in case HasSingle.elem elem_in_child of
				Right ts -> 
					let
						new_elem :: Elem
						new_elem = Data.Image (elem_in_child $> (), Ts.render ts) <$ trunk
						in Right (Tree.Node new_elem [])
				_ -> error (Tree.rootLabel single_child)
					"child of picture node \
					\has to express the file path of the picture as a regular text node\
					\, but is a meta node"
		_ : _ : _ -> error trunk "picture node should have only 1 child, for file path"

parse :: WholeIntermediate -> Parsed Whole
parse (tree@ (Tree.Node trunk children)) = case HasSingle.elem trunk of
	Left mn -> case mn of
		MnSpace -> map (Tree.Node (Data.Text (Right " ") <$ trunk)) (traverse parse children)
		MnImage -> parse_image tree
	Right normal_text -> map (Tree.Node (Data.Text normal_text <$ trunk)) (traverse parse children)

render :: Whole -> WholeIntermediate
render (Tree.Node trunk children) = case HasSingle.elem trunk of
	Data.Text t -> Tree.Node (Right t <$ trunk) (map render children)
	Data.Image file_path_elem -> let
		file_path_node :: WholeIntermediate
		file_path_node = Tree.Node (HasSingle.move_in (map (Right >>> Right) file_path_elem)) []
		in Tree.Node (trunk $> Left MnImage) [file_path_node]

layer_tree :: Optic.PartialIso' ParseError WholeIntermediate Whole
layer_tree = Optic.PartialIso render parse


layer :: Optic.PartialIso' ParseError (Tree (Label.Elem Text Ts.Content')) Whole
layer = convert_from_describing_class_4 layer_in_node >**> layer_tree
