module WriteInTree.Document.Core.Serial.RichTextTree.Label.InlineClassCoding
(
	layer,
)
where

import Data.Functor ((<$))
import Data.Functor.Identity (Identity (..))
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Elem
import WriteInTree.Document.Core.Serial.RichTextTree.Label.TextSplit (ClassName, Configuration)
import WriteInTree.Document.SepProps.Data (InlineClass (..))

import qualified Control.Monad.State.Strict as Mtl
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Maybe as Base
import qualified Fana.Data.HasSingle as HasSingle
import qualified Fana.Data.HasSingle as Fana
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.Key.LensToMaybeElement as Map
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Intermediate as Intermediate
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.TextSplit as TextSplit


type Char = Base.Char
type Text = [Char]

type ElemTT = Elem Text Text

layer_text_split :: Configuration -> Optic.Iso' ElemTT (Elem Text TextSplit.H)
layer_text_split config = Optic.lift_iso (TextSplit.layer config)

-- ~ over_Elem :: Optic.Iso' (Elem l TextSplit.H) ([ClassName], Elem l Text)
-- ~ over_Elem = Base.sequence
	-- ~ let
		-- ~ render :: ([ClassName], Either l Text) -> Either l TextSplit.H
		-- ~ render (cs, ei) = Base.either Left (Pair.after cs >>> Right) ei
		-- ~ parse :: Either l TextSplit.H -> ([ClassName], Either l Text)
		-- ~ parse = Base.either (Left >>> Pair.after []) (map Right)
		-- ~ in Optic.Iso render parse

over_has_single :: forall e c . Fana.HasSingle c => Optic.Iso' (c ([ClassName], e)) ([ClassName], c e)
over_has_single = let
	render :: ([ClassName], c e) -> (c ([ClassName], e))
	render (cs, c) = map (Pair.after cs) c
	parse :: (c ([ClassName], e)) -> ([ClassName], c e)
	parse c = let 
		single = HasSingle.elem c
		e = snd single
		in (e <$ c) <$ single
	in Optic.Iso render parse

move_class_out_from_container :: forall e c . ClassName -> Optic.AffineTraversal' (Maybe e) c -> c -> (Maybe ClassName, c)
move_class_out_from_container class_name at c = let
	when_there :: Maybe e -> (Maybe Text, c)
	when_there e_mb = if Base.isJust e_mb 
		then (Just class_name, Optic.fill at Nothing c)
		else (Nothing, c)
	in Base.either (const (Nothing, c)) when_there (Optic.match at c)

move_class_out_from_elem :: ClassName -> Elem Text e -> (Maybe ClassName, Elem Text e)
move_class_out_from_elem class_name = let
	traversal = Category2.empty
		>**>^ Map.lens_at class_name 
		>**>^ Optic.prism_Maybe 
		>**>^ Intermediate.ofLabels_classes
		>**>^ inElem_labels
	in move_class_out_from_container class_name traversal

move_class_out_from_elem_st :: ClassName -> Mtl.State (Elem Text e) (Maybe ClassName)
move_class_out_from_elem_st = move_class_out_from_elem >>> map Identity >>> Mtl.StateT

move_classes_out_from_elem_st :: forall e . Configuration -> Mtl.State (Elem Text e) [Maybe ClassName]
move_classes_out_from_elem_st = traverse (ilc_name >>> move_class_out_from_elem_st)

move_classes_out_from_elem :: forall e . Configuration -> Elem Text e -> ([ClassName], Elem Text e)
move_classes_out_from_elem = id
	>>> move_classes_out_from_elem_st 
	>>> Mtl.runState
	>>> map (Bifunctor.first Base.catMaybes)

move_classes_out_from_elem' :: Configuration -> Elem Text Text -> Elem Text TextSplit.H
move_classes_out_from_elem' config =
	move_classes_out_from_elem config >>>
	(\ (classes, elem) -> map (Pair.after classes) elem)

over_Elem :: Configuration -> Optic.Iso' ([ClassName], Elem Text e) (Elem Text e)
over_Elem config = let
	parse :: ([ClassName], Elem Text e) -> Elem Text e
	parse (cs, elem) = Optic.fn_up inElem_labels (Intermediate.add_new_classes_to_Labels cs) elem
	in Optic.Iso (move_classes_out_from_elem config) parse

over_Elem' :: Configuration -> Optic.Iso' (Elem Text TextSplit.H) (Elem Text Text)
over_Elem' config =
	let
		parse :: Elem Text TextSplit.H -> Elem Text Text
		parse elem =
			case ofElem_core elem of
				(cs, text) -> Optic.fn_up inElem_labels (Intermediate.add_new_classes_to_Labels cs) (map snd elem)
		in Optic.Iso (move_classes_out_from_elem' config) parse

layer :: Configuration -> Optic.Iso' ElemTT ElemTT
layer config = Optic.lift_iso (TextSplit.layer config) >**> over_Elem' config
