module WriteInTree.Document.Core.Serial.Paragraph
(
	layer,
)
where

import Data.Tree (Tree)
import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude

import qualified Data.Tree as Tree
import qualified Fana.Data.HasSingle as Fana
import qualified Fana.Data.HasSingle as HasSingle
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String

type Inline = Data.Inline Text
type A = Label.Elem Text
type Paragraph = Data.Paragraph Text


layer_move_additional_info :: 
	(Fana.HasSingle a1, Fana.HasSingle a2) => 
	Optic.Iso (Tree (a1 e1)) (Tree (a2 e2)) (Tree (a1 (), e1)) (Tree (a2 (), e2))
layer_move_additional_info = Optic.iso_up HasSingle.iso_separate

type CoreElemL a = (a (), Inline)
type CoreElemH a = (a (), Paragraph)

type CoreL a = Tree (CoreElemL a)
type CoreH a = Tree (CoreElemH a)

parse_core :: forall a . (forall x . Pos.HasPosition (a x)) => CoreL a -> CoreH a
parse_core tree = let
	trunk :: CoreElemL a
	trunk = Tree.rootLabel tree
	(a, trunk_inline {- :: Inline a Text -}) = trunk
	processed_children = map parse_core (Tree.subForest tree)
	when_normal :: Inline -> [CoreH a] -> CoreH a
	when_normal inline = let
		new_trunk :: CoreElemH a
		new_trunk = (a, inline)
		in Tree.Node new_trunk
	in when_normal trunk_inline processed_children

render_core :: forall a . CoreH a -> CoreL a
render_core tree = let
	Tree.Node trunk children = tree
	(a, paragraph) = trunk -- :: (a (), Paragraph a)
	render_any_paragraph :: a () -> Paragraph -> CoreL a
	render_any_paragraph a' inline = 
		-- in this case the additional info of the paragraph was created during parsing
		-- to be the same as the additional info of the inline,
		-- outside may have changed this additional info, the instance attached to the paragraph;
		-- hence i take that one here to live on and drop the one attached to the inline;
		-- but in theory the one of the inline may have got changed too, thus this is dodgy
		Tree.Node (a', inline) (map render_core children)
	in render_any_paragraph a paragraph

layer_core :: forall a . (forall x . Pos.HasPosition (a x)) => Optic.Iso' (CoreL a) (CoreH a)
layer_core = Optic.Iso render_core parse_core

layer_general :: 
	forall a . (forall x . Pos.HasPosition (a x), Fana.HasSingle a) => 
	Optic.Iso' (Tree (a Inline)) (CoreH a)
layer_general = 
	convert_from_describing_class_4 layer_move_additional_info
	>**>
	layer_core

layer :: Optic.Iso' (Tree (A Inline)) (CoreH A)
layer = layer_general
