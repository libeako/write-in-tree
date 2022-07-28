module WriteInTree.Document.Core.Serial.InlineContent
(
	Elem,
	layer
)
where

import Data.Functor ((<$))
import Data.Tree (Tree (..))
import Fana.Prelude

import qualified Data.Tree as Tree
import qualified Fana.Data.HasSingle as HasSingle
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.TextStructure as Ts
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String


type ElemIntermediate = Label.Elem Text Ts.Content'
type Elem = Label.Elem Text (Data.InlineVisual Ts.Content')

type ParseError = Pos.Positioned (Accu.Accumulated Text)
type Parsed = Either ParseError

render :: Tree Elem -> Tree ElemIntermediate
render (Tree.Node trunk children) =
	case HasSingle.elem trunk of
		Data.Text t -> Tree.Node (t <$ trunk) (map render children)

parse :: Tree ElemIntermediate -> Parsed (Tree Elem)
parse (tree@(Tree.Node trunk children)) =
	case HasSingle.elem trunk of
		normal_text -> map (Tree.Node (Data.Text normal_text <$ trunk)) (traverse parse children)

layer :: Optic.PartialIso' ParseError (Tree (Label.Elem Text Ts.Content')) (Tree Elem)
layer = Optic.PartialIso render parse
