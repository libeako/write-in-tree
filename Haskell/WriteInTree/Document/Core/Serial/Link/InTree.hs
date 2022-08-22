module WriteInTree.Document.Core.Serial.Link.InTree
(
	layer,
)
where

import Data.Functor ((<$))
import Data.Tree (Tree (..))
import Fana.Data.HasSingle (HasSingle)
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude
import WriteInTree.Document.Core.Serial.Link.Individual (MetaNodeName (..))

import qualified Fana.Data.HasSingle as HasSingle
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.Link.Individual as Individual
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.MetaStructure as Ms
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String
type A = Label.Elem Text -- additional info wrapper
type Inline = Data.Inline Text


render_MetaNodeName :: MetaNodeName -> Text
render_MetaNodeName = \case { MnLink -> "links-to" }

type ParseError = Pos.Positioned (Accu.Accumulated Text)

parse_tree :: forall a . HasSingle a => Tree (a Text) -> Tree (a Text, Maybe (a MetaNodeName, [Tree (a Text)]))
parse_tree =
	let
		extract_meta_name :: a Text -> Maybe MetaNodeName
		extract_meta_name =
			HasSingle.elem >>>
			Optic.ofIso_up (Ms.layer_in_node_text' render_MetaNodeName) >>>
			either (Just) (const Nothing)
		diagonal x = (x, x)
		decide :: a Text -> Maybe (a MetaNodeName)
		decide elem = map (<$ elem) (extract_meta_name elem)
		in
			\ (Node r c) ->
				case (decide r) of
					Just mn -> Node (r, Just (mn, c)) (map (map (Pair.before Nothing)) c)
					Nothing -> Node (r, Nothing) (map parse_tree c)

layer_tree :: forall a . HasSingle a => Optic.Iso' (Tree (a Text)) (Tree (a Text, Maybe (a MetaNodeName, [Tree (a Text)])))
layer_tree = Optic.Iso (map fst) parse_tree


attach_link_to_visual :: (A Text, Maybe (Data.Link Text)) -> A (Inline)
attach_link_to_visual (visual, link) = map (flip Data.Inline link) visual

detach_link_to_visual :: A Inline -> (A Text, Maybe (Data.Link Text))
detach_link_to_visual i =
	let
		inline :: Inline
		inline = HasSingle.elem i
		in (Data.ilVisual inline <$ i, Data.ilLink inline)

layer_tach_link_to_visual :: 
	Optic.Iso' (A Text, Maybe (Data.Link Text)) (A Inline)
layer_tach_link_to_visual = Optic.Iso detach_link_to_visual attach_link_to_visual


layer :: Optic.PartialIso' ParseError (Tree (A Text)) (Tree (A Inline))
layer =
	Category2.empty
	>**>^ layer_tree
	>**>^ (Optic.lift_piso >>> Optic.lift_piso >>> Optic.lift_piso) Individual.layer
	>**>^ Optic.lift_iso layer_tach_link_to_visual
