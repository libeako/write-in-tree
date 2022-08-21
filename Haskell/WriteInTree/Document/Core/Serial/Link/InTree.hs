module WriteInTree.Document.Core.Serial.Link.InTree
(
	ElemH,
	layer,
)
where

import Data.Functor ((<$))
import Data.Tree (Tree (..))
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import WriteInTree.Document.Core.Serial.Link.Individual (MetaNodeName (..))

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Either as Either
import qualified Data.Foldable as Base
import qualified Data.Tree as Tree
import qualified Fana.Data.HasSingle as Fana
import qualified Fana.Data.HasSingle as HasSingle
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.Link.Individual as Individual
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.MetaStructure as Ms
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.TextStructure as Ts
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String
type A = Label.Elem Text -- additional info wrapper
type AB = (,) (A ())
type Inline = Data.Inline Text
type InputTreeOfLink l r = (l, [Tree (Either l r)])
type InputTreeOfLink' = InputTreeOfLink (A MetaNodeName) (A Ts.Content')
type LinksSeparated l r = Tree (r, [(l, [Tree (Either l r)])])
type LinkSeparated l r = Tree (r, Maybe (l, [Tree (Either l r)]))
type ElemL = A Ts.Content'
type ContainerL e = Tree (A e)
type ElemEither = Either (A MetaNodeName) (A Ts.Content')
type ElemH = ElemL
type WholeL = ContainerL Ts.Content'


render_MetaNodeName :: MetaNodeName -> Text
render_MetaNodeName = \case { MnLink -> "links-to" }


layer_first :: Optic.PartialIso' () [e] (Maybe e)
layer_first = let
	parse :: [e] -> Either () (Maybe e)
	parse = \case
		[] -> Right Nothing
		[e] -> Right (Just e)
		e1 : e2 : _ -> Left ()
	in Optic.PartialIso Base.toList parse

layer_meta_name_tree :: Optic.Iso' (ContainerL Ts.Content') (ContainerL (Either MetaNodeName Ts.Content'))
layer_meta_name_tree = 
	(Optic.iso_up >>> Optic.iso_up)
		(Ms.layer_in_node_text render_MetaNodeName)

layer_MetaName :: Optic.Iso' (ContainerL Text) (ContainerL (Either MetaNodeName Text))
layer_MetaName =
	(Optic.iso_up >>> Optic.iso_up)
		(Ms.serialize_node_content_without_worry Just id render_MetaNodeName)

layer_inside_elem :: Optic.Iso' Ts.Content' (Either MetaNodeName Ts.Content')
layer_inside_elem = Ms.layer_in_node_text render_MetaNodeName

layer_Either :: forall a l r . Fana.HasSingle a => Optic.Iso' (a (Either l r)) (Either (a l) (a r))
layer_Either = let
	parse :: a (Either l r) -> Either (a l) (a r)
	parse wrapped = Base.either ((<$ wrapped) >>> Left) ((<$ wrapped) >>> Right) (HasSingle.elem wrapped)
	render = Base.either (map Left) (map Right)
	in Optic.Iso render parse

layer_inside_node :: 
	Fana.HasSingle a => 
	Optic.Iso' (a Ts.Content') (Either (a MetaNodeName) (a Ts.Content'))
layer_inside_node = Optic.iso_up layer_inside_elem >**> layer_Either


type ParseError = Pos.Positioned (Accu.Accumulated Text)


separate_links :: Tree (Either l r) -> Either (l, [Tree (Either l r)]) (LinksSeparated l r)
separate_links (Tree.Node node children) = case node of
	Left l -> Left (l, children)
	Right r -> let
		(ls, rs) = Either.partitionEithers (map separate_links children)
		in Right (Tree.Node (r, ls) rs)

separate_links_in_whole_tree :: l ~ A e => Tree (Either l r) -> Either ParseError (LinksSeparated l r)
separate_links_in_whole_tree = separate_links 
	>>> Bifunctor.first 
		(
			id
			>>> fst >>> Pos.get_position
			>>> flip Pos.Positioned "link node must have a parent inline"
		)

link_case_to_tree :: (l, [Tree (Either l r)]) -> Tree (Either l r)
link_case_to_tree (l, children) = Tree.Node (Left l) children

merge_links :: (LinksSeparated l r) -> Tree (Either l r)
merge_links (Tree.Node (r, link_children) regular_children) = 
	Tree.Node (Right r) (map link_case_to_tree link_children <> (map merge_links regular_children))

layer_link_separation :: 
	l ~ A e => Optic.PartialIso' ParseError (Tree (Either l r)) (LinksSeparated l r)
layer_link_separation = Optic.PartialIso merge_links separate_links_in_whole_tree


layer_first_link_of_node :: forall r e something . r ~ A something => Optic.PartialIso' ParseError (r, [e]) (r, Maybe e)
layer_first_link_of_node = let
	render = map (Optic.down layer_first)
	parse :: (r, [e]) -> Either ParseError (r, Maybe e)
	parse (r, list) = let
		error :: ParseError
		error = Pos.Positioned (Pos.get_position r) "node has multiple link nodes"
		in Bifunctor.bimap (const error) (Pair.after r) (Optic.piso_interpret layer_first list)
	in Optic.PartialIso render parse


attach_link_to_visual :: (A Text, Maybe (Data.Link Text)) -> A (Inline Text)
attach_link_to_visual (visual, link) = map (flip Data.Inline link) visual

detach_link_to_visual :: A (Inline Text) -> (A Text, Maybe (Data.Link Text))
detach_link_to_visual i =
	let
		inline :: Inline Text
		inline = HasSingle.elem i
		in (Data.ilVisual inline  <$ i, Data.ilLink inline)

layer_tach_link_to_visual :: 
	Optic.Iso' (A Text, Maybe (Data.Link Text)) (A (Inline Text))
layer_tach_link_to_visual = Optic.Iso detach_link_to_visual attach_link_to_visual


layer_forget :: Optic.Iso' (A Ts.Content', b) (A Text, b)
layer_forget = Optic.iso_pair_swap >**> (Optic.iso_up >>> Optic.iso_up) Ms.forget_about_meta >**> Optic.iso_pair_swap

layer :: Optic.PartialIso' ParseError WholeL (Tree (A (Inline Text)))
layer = Category2.empty
	>**>^ (Optic.iso_up layer_inside_node) 
	>**>^ layer_link_separation
	>**>^ Optic.lift_piso layer_first_link_of_node
	>**>^ (Optic.lift_piso >>> Optic.lift_piso >>> Optic.lift_piso) Individual.layer
	>**>^ (Optic.iso_up (layer_forget >**> layer_tach_link_to_visual))
