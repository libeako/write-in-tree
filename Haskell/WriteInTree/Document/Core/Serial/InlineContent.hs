module WriteInTree.Document.Core.Serial.InlineContent
(
	Elem,
	Whole, layer
)
where

import Data.Functor ((<$))
import Data.Tree (Tree (..))
import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude

import qualified Data.Tree as Tree
import qualified Fana.Data.HasSingle as HasSingle
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.TextStructure as Ts
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String


type ElemIntermediate = Label.Elem Text Ts.Content'
type Elem = Label.Elem Text (Data.InlineVisual (Label.Elem Text ()) Ts.Content')
type WholeIntermediate = Tree ElemIntermediate
type Whole = Tree Elem

layer_in_node :: Optic.Iso' (Tree (Label.Elem Text Ts.Content')) WholeIntermediate
layer_in_node = Category2.empty


type ParseError = Pos.Positioned (Accu.Accumulated Text)
type Parsed = Either ParseError

parse :: WholeIntermediate -> Parsed Whole
parse (tree@(Tree.Node trunk children)) =
	case HasSingle.elem trunk of
		normal_text -> map (Tree.Node (Data.Text normal_text <$ trunk)) (traverse parse children)

render :: Whole -> WholeIntermediate
render (Tree.Node trunk children) =
	case HasSingle.elem trunk of
		Data.Text t -> Tree.Node (t <$ trunk) (map render children)

layer_tree :: Optic.PartialIso' ParseError WholeIntermediate Whole
layer_tree = Optic.PartialIso render parse


layer :: Optic.PartialIso' ParseError (Tree (Label.Elem Text Ts.Content')) Whole
layer = convert_from_describing_class_4 layer_in_node >**> layer_tree
