module WriteInTree.Document.Core.Serial.Link.Individual
(
	MetaNodeName (..),
	Concrete,
	WholeH,
	layer,
)
where


import Data.Tree (Tree (..))
import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Tree as Tree
import qualified Fana.Data.HasSingle as HasSingle
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Instances.Enum as Serial
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.TextStructure as Ts
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Elem as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String
type A = Label.Elem Text -- additional info wrapper
type AB = (,) (A ())

data MetaNodeName = MnLink deriving (Base.Enum, Base.Bounded)

type Concrete t = t (A MetaNodeName) (A Ts.Content')
type WholeL l r = (l, [Tree (Either l r)])
type WholeL' = Concrete WholeL
type ParseError = Pos.Positioned (Accu.Accumulated Text)


layer_text_structure :: Optic.PartialIso' (Accu.Accumulated Text) Ts.Content' Text
layer_text_structure = let
	error_description = "descendants of a link header node must be of type regular"
	in Optic.PartialIso Right (Bifunctor.first (const (Accu.single error_description)))
layer_text_structure_in_A :: Optic.PartialIso' ParseError (A Ts.Content') (A Text)
layer_text_structure_in_A =
	Optic.piso_convert_error_with_low Pos.position_error 
		((Optic.lift_piso) layer_text_structure)

layer_either :: forall l r e . l ~ A e => Optic.PartialIso' ParseError (Either l r) r
layer_either = let
	error :: l -> ParseError
	error = id 
		>>> Pos.get_position 
		>>> flip Pos.Positioned "node under link head node must not be link head"
	in Optic.PartialIso Right (Bifunctor.first error)

layer_either_whole :: 
	l ~ A e => 
	Optic.PartialIso' ParseError (Either l (A Ts.Content')) (A Text)
layer_either_whole = layer_either >**> layer_text_structure_in_A


data DestinationType = Internal | External deriving (Base.Enum, Base.Bounded)

render_DestinationType :: DestinationType -> Text
render_DestinationType = \case
	Internal -> "internal"
	External -> "external"

layer_destination_type :: Optic.PartialIso' (Accu.Accumulated Text) Text DestinationType
layer_destination_type = let
	error_description = "link destination type not recognized"
	in 
		Optic.piso_convert_error_with_low (const (const error_description)) 
			(Serial.enum render_DestinationType)

lift_piso_to_weird :: forall e l h . Optic.PartialIso' e l h -> Optic.PartialIso' e (AB l) h
lift_piso_to_weird piso =
	let
		render :: h -> AB l
		render = Optic.piso_down piso >>> Pair.after (Label.default_Elem_context ())
		parse :: AB l -> Either e h
		parse = snd >>> Optic.piso_interpret piso
		in Optic.PartialIso render parse

layer_destination_type_whole :: Optic.PartialIso' ParseError (AB Text) DestinationType
layer_destination_type_whole =
	Optic.piso_convert_error_with_low (fst >>> Pos.position_error) (lift_piso_to_weird layer_destination_type)

type Core = (AB Text, AB Text)

parse_into_core :: [Tree e] -> Either (Accu.Accumulated Text) (e, e)
parse_into_core = \case
	[Tree.Node node_1 _, Tree.Node node_2 _] -> Right (node_1, node_2)
	_ -> Left (Accu.single "a link node must have exactly 2 children [target type, target address]")

render_from_core :: (e, e) -> [Tree e]
render_from_core (node_1, node_2) = [Tree.Node node_1 [], Tree.Node node_2 []]

layer_core_l :: Optic.PartialIso' (Accu.Accumulated Text) [Tree e] (e, e)
layer_core_l = Optic.PartialIso render_from_core parse_into_core

layer_core_l_whole :: Optic.PartialIso' (Accu.Accumulated Text) [Tree (A Text)] Core
layer_core_l_whole = 
	convert_from_describing_class_4 ((Optic.iso_up >>> Optic.iso_up) HasSingle.iso_separate)
	>**> layer_core_l
	
type CoreSmart = (DestinationType, AB Text)

layer_smart :: Optic.PartialIso' ParseError Core CoreSmart
layer_smart = Category2.empty 
	>**>^ Optic.iso_pair_swap
	>**>^ Optic.lift_piso layer_destination_type_whole
	>**>^ Optic.iso_pair_swap

parse_from_core :: DestinationType -> AB Text -> Data.Link Text
parse_from_core d =
	snd >>> case d of { Internal -> Data.LIn; External -> Data.LEx }

render_to_core :: Data.Link Text -> (DestinationType, AB Text)
render_to_core =
	\case
		Data.LIn a -> (Internal, a)
		Data.LEx a -> (External, a)
	>>> map (Pair.after (Label.default_Elem_context ()))

layer_core_h :: Optic.Iso' CoreSmart (Data.Link Text)
layer_core_h = Optic.Iso render_to_core (uncurry parse_from_core)

layer_core :: Pos.HasPosition l => Optic.PartialIso' ParseError (l, [Tree (A Text)]) (l, Data.Link Text)
layer_core = Category2.empty
	>**> Optic.piso_convert_error_with_low 
		(\ i e -> Pos.position_error (fst i) e) 
		(Optic.lift_piso layer_core_l_whole)
	>**> Optic.lift_piso (layer_smart >**>^ layer_core_h)


render_trunk :: x -> (A MetaNodeName, x)
render_trunk = Pair.after (Label.default_Elem_context MnLink)

layer_with_trunk :: Optic.Iso' (A MetaNodeName, x) x
layer_with_trunk = Optic.Iso render_trunk snd


layer_general :: 
	l ~ A MetaNodeName => 
	Optic.PartialIso' ParseError (l, [Tree (Either l (A Ts.Content'))]) (Data.Link Text)
layer_general = Category2.empty
	>**>^ (Optic.lift_piso >>> Optic.lift_piso >>> Optic.lift_piso) layer_either_whole 
	>**>^ layer_core 
	>**>^ layer_with_trunk


type WholeH l r = Data.Link Text
type WholeH' = WholeH (A MetaNodeName) (A Text)


layer :: Optic.PartialIso' ParseError WholeL' WholeH'
layer = layer_general
