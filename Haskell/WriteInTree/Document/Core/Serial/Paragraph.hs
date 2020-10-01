module WriteInTree.Document.Core.Serial.Paragraph
(
	layer, 
	ElemH, H, CoreH,
)
where

import Control.Applicative ((<*))
import Control.Monad ((>=>))
import Data.Tree (Tree)
import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import Prelude (fst, snd, sequenceA)

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Tree as Tree
import qualified Fana.Data.HasSingle as Fana
import qualified Fana.Data.HasSingle as HasSingle
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.MetaName as Mn
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.MetaStructure as Ms
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.TextStructure as Ts
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String

type Inline a e = Data.Inline (a ()) (a ()) Text e
type InputElem a e = a (Inline a e)
type InputElemPair a e = (a (), Inline a e)
type OutputElem a e = (a (), Data.Paragraph (a ()) (a ()) Text e)
type A = Label.Elem Text
type Paragraph a = Data.Paragraph (a ()) (a ()) Text Text


layer_move_additional_info :: 
	(Fana.HasSingle a1, Fana.HasSingle a2) => 
	Optic.Iso (Tree (a1 e1)) (Tree (a2 e2)) (Tree (a1 (), e1)) (Tree (a2 (), e2))
layer_move_additional_info = Optic.iso_up HasSingle.iso_separate


data MetaNodeName = MnParagraph deriving (Base.Enum, Base.Bounded)

render_MetaNodeName :: MetaNodeName -> Text
render_MetaNodeName = \case { MnParagraph -> "paragraph" }

layer_either :: Optic.Iso' (Inline a Ts.Content') (Either Text (Inline a Text))
layer_either = let
	down = Base.either (Left >>> Data.Text >>> flip Data.Inline Nothing) (map Right)
	in Optic.Iso down sequenceA

type CoreElemL a = (a (), Either (Either MetaNodeName Text) (Inline a Text))
type CoreElemH a = (a (), Either Text (Paragraph a))

layer_meta_node' :: Optic.Iso' (Either Text (Inline a Text)) (Either (Either MetaNodeName Text) (Inline a Text))
layer_meta_node' = Ms.lift_iso_to_Left (Mn.iso render_MetaNodeName)

layer_meta_node :: Optic.Iso' (a (), Either Text (Inline a Text)) (CoreElemL a)
layer_meta_node = Optic.iso_up layer_meta_node'


type ParseError = Pos.Positioned (Accu.Accumulated Text)

type CoreL a = Tree (CoreElemL a)
type CoreH a = Tree (CoreElemH a)
type CoreParseResult a = Either ParseError (CoreH a)

parse_single_inline_under_paragraph :: Paragraph a -> Either (Accu.Accumulated Text) (a (), Inline a Text)
parse_single_inline_under_paragraph = \case
	[s] -> Right s
	_ -> Left "under a paragraph node all nodes should contain exactly 1 inline"
parse_single_inline_under_paragraph' :: 
	(forall x . Pos.HasPosition (a x)) => 
	(a(), Paragraph a) -> Either ParseError (a (), Inline a Text)
parse_single_inline_under_paragraph' (a, paragraph) = Bifunctor.first 
	(Pos.Positioned (Pos.get_position a))
	(parse_single_inline_under_paragraph paragraph)

parse_regular_paragraph_under_paragraph :: Either Text p -> Either (Accu.Accumulated Text) p
parse_regular_paragraph_under_paragraph = Bifunctor.first
	(const "all nodes under a paragraph node must represent regular inlines")
parse_regular_paragraph_under_paragraph' ::
	(forall x . Pos.HasPosition (a x)) =>
	(a (), Either Text p) -> Either ParseError (a (), p)
parse_regular_paragraph_under_paragraph' (a, ei_p) = Bifunctor.bimap
	(Pos.Positioned (Pos.get_position a))
	(Pair.after a)
	(parse_regular_paragraph_under_paragraph ei_p)

parse_regular_single_inline_under_paragraph ::
	(forall x . Pos.HasPosition (a x)) => 
	CoreElemH a -> Either ParseError (a (), Inline a Text)
parse_regular_single_inline_under_paragraph = pure
	 >=> parse_regular_paragraph_under_paragraph'
	 >=> parse_single_inline_under_paragraph'

tree_is_flat_under_paragraph :: 
	forall a . (forall x . Pos.HasPosition (a x)) => 
	Tree (CoreElemH a) -> Either ParseError ()
tree_is_flat_under_paragraph tree = case Tree.subForest tree of
	[] -> Right ()
	_ -> let
		a = fst (Tree.rootLabel tree)
		error_description = "all nodes under a paragraph node must represent a shallow inline element"
		in Left (Pos.Positioned (Pos.get_position a) error_description)

forest_is_flat_under_paragraph :: 
	forall a . (forall x . Pos.HasPosition (a x)) => 
	[Tree (CoreElemH a)] -> Either ParseError ()
forest_is_flat_under_paragraph = traverse tree_is_flat_under_paragraph >>> map (const ())

parse_valid_inlines_under_paragraph ::
	forall a . (forall x . Pos.HasPosition (a x)) => 
	a () -> [Tree (CoreElemH a)] -> CoreParseResult a
parse_valid_inlines_under_paragraph a children = let 
	inlines :: Either ParseError [(a (), Inline a Text)]
	inlines = traverse (Tree.rootLabel >>> parse_regular_single_inline_under_paragraph) children
	in 
		map (Right >>> Pair.after a >>> flip Tree.Node []) inlines
		<* (forest_is_flat_under_paragraph children)

parse_core :: forall a . (forall x . Pos.HasPosition (a x)) => CoreL a -> CoreParseResult a
parse_core tree = let
	trunk :: CoreElemL a
	trunk = Tree.rootLabel tree
	(a, trunk_either_inline) = trunk
	processed_children = traverse parse_core (Tree.subForest tree)
	when_normal :: Inline a Text -> [CoreH a] -> CoreParseResult a
	when_normal inline = let
		new_trunk :: CoreElemH a
		new_trunk = (a, Right [(a, inline)])
		in Tree.Node new_trunk >>> Right
	when_paragraph :: [Tree (CoreElemH a)] -> CoreParseResult a
	when_paragraph = parse_valid_inlines_under_paragraph a
	when_foreigh_meta :: a () -> Text -> [Tree (CoreElemH a)] -> CoreParseResult a
	when_foreigh_meta a' text = Tree.Node (a', Left text) >>> Right
	in 
		processed_children >>= 
		Base.either (Base.either (const when_paragraph) (when_foreigh_meta a)) when_normal trunk_either_inline

render_paragraph :: forall a . (a (), Paragraph a) -> CoreL a
render_paragraph (a, inlines) = let
	children :: [CoreL a]
	children = let
		per_child :: (a (), Data.Inline (a ()) (a ()) Text Text) -> CoreL a
		per_child child = Tree.Node (map Right child) []
		in map per_child inlines
	trunk :: CoreElemL a
	trunk = (a, Left (Left MnParagraph))
	in Tree.Node trunk children

render_core :: forall a . CoreH a -> CoreL a
render_core tree = let
	Tree.Node trunk children = tree
	(a, ei_paragraph) = trunk
	render_any_paragraph :: a () -> Paragraph a -> CoreL a
	render_any_paragraph a' p = case p of
		_ : _ : _ -> render_paragraph (a', p)
		[inline] -> 
			-- in this case the additional info of the paragraph was created during parsing
			-- to be the same as the additional info of the inline,
			-- outside may have changed this additional info, the instance attached to the paragraph;
			-- hence i take that one here to live on and drop the one attached to the inline;
			-- but in theory the one of the inline may have got changed too, thus this is dodgy
			Tree.Node (a', Right (snd inline)) (map render_core children)
		_ -> Base.error "internal error detected [paragraph without inline]"
	in Base.either 
		(Right >>> Left >>> Pair.after a >>> flip Tree.Node (map render_core children))
		(render_any_paragraph a)
		ei_paragraph

layer_core :: forall a . (forall x . Pos.HasPosition (a x)) => Optic.PartialIso' ParseError (CoreL a) (CoreH a)
layer_core = Optic.PartialIso render_core parse_core



layer_general :: 
	forall a . (forall x . Pos.HasPosition (a x), Fana.HasSingle a) => 
	Optic.PartialIso' ParseError (Tree (a (Inline a Ts.Content'))) (CoreH a)
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

layer :: Optic.PartialIso' ParseError (Tree (A (Inline A Ts.Content'))) H
layer = layer_general
