module WriteInTree.Document.Core.Serial.Paragraph
(
	layer, 
	ElemH, H, CoreH,
)
where

import Data.Tree (Tree)
import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude

import qualified Data.Tree as Tree
import qualified Fana.Data.HasSingle as Fana
import qualified Fana.Data.HasSingle as HasSingle
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.TextStructure as Ts
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String

type Inline e = Data.Inline Text e
type InputElem a e = a (Inline e)
type InputElemPair a e = (a (), Inline e)
type OutputElem a e = (a (), Data.Paragraph Text e)
type A = Label.Elem Text
type Paragraph = Data.Paragraph Text Text


layer_move_additional_info :: 
	(Fana.HasSingle a1, Fana.HasSingle a2) => 
	Optic.Iso (Tree (a1 e1)) (Tree (a2 e2)) (Tree (a1 (), e1)) (Tree (a2 (), e2))
layer_move_additional_info = Optic.iso_up HasSingle.iso_separate

layer_either :: Optic.Iso' (Inline Ts.Content') (Either Text (Inline Text))
layer_either = let
	down = Base.either (Left >>> flip Data.Inline Nothing) (map Right)
	in Optic.Iso down sequenceA

type CoreElemL a = (a (), Either Text (Inline Text))
type CoreElemH a = (a (), Either Text Paragraph)

layer_meta_node' :: Optic.Iso' (Either Text (Inline Text)) (Either Text (Inline Text))
layer_meta_node' = Category2.empty

layer_meta_node :: Optic.Iso' (a (), Either Text (Inline Text)) (CoreElemL a)
layer_meta_node = Optic.iso_up layer_meta_node'


type CoreL a = Tree (CoreElemL a)
type CoreH a = Tree (CoreElemH a)

parse_core :: forall a . (forall x . Pos.HasPosition (a x)) => CoreL a -> CoreH a
parse_core tree = let
	trunk :: CoreElemL a
	trunk = Tree.rootLabel tree
	(a, trunk_either_inline {- :: Either Text (Inline a Text) -}) = trunk
	processed_children = map parse_core (Tree.subForest tree)
	when_normal :: Inline Text -> [CoreH a] -> CoreH a
	when_normal inline = let
		new_trunk :: CoreElemH a
		new_trunk = (a, Right inline)
		in Tree.Node new_trunk
	when_foreigh_meta :: a () -> Text -> [Tree (CoreElemH a)] -> CoreH a
	when_foreigh_meta a' text = Tree.Node (a', Left text)
	in Base.either (when_foreigh_meta a) when_normal trunk_either_inline processed_children

render_core :: forall a . CoreH a -> CoreL a
render_core tree = let
	Tree.Node trunk children = tree
	(a, ei_paragraph) = trunk -- :: (a (), Either Text (Paragraph a))
	render_any_paragraph :: a () -> Paragraph -> CoreL a
	render_any_paragraph a' inline = 
		-- in this case the additional info of the paragraph was created during parsing
		-- to be the same as the additional info of the inline,
		-- outside may have changed this additional info, the instance attached to the paragraph;
		-- hence i take that one here to live on and drop the one attached to the inline;
		-- but in theory the one of the inline may have got changed too, thus this is dodgy
		Tree.Node (a', Right inline) (map render_core children)
	in Base.either 
		(Left >>> Pair.after a >>> flip Tree.Node (map render_core children))
		(render_any_paragraph a)
		ei_paragraph

layer_core :: forall a . (forall x . Pos.HasPosition (a x)) => Optic.Iso' (CoreL a) (CoreH a)
layer_core = Optic.Iso render_core parse_core

layer_general :: 
	forall a . (forall x . Pos.HasPosition (a x), Fana.HasSingle a) => 
	Optic.Iso' (Tree (a (Inline Ts.Content'))) (CoreH a)
layer_general = 
	convert_from_describing_class_4
		(
			layer_move_additional_info
			>**>
			((Optic.iso_up >>> Optic.iso_up) (layer_either >**> layer_meta_node')) 
		)
	>**>
	layer_core

type ElemH = CoreElemH A
type H = CoreH A

layer :: Optic.Iso' (Tree (A (Inline Ts.Content'))) H
layer = layer_general
