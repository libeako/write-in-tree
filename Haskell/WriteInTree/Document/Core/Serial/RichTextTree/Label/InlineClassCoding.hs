module WriteInTree.Document.Core.Serial.RichTextTree.Label.InlineClassCoding
(
	layer,
)
where

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


move_class_out_from_container :: forall e c . ClassName -> Optic.AffineTraversal' (Maybe e) c -> c -> (Maybe ClassName, c)
move_class_out_from_container class_name at c = let
	when_there :: Maybe e -> (Maybe Text, c)
	when_there e_mb = if Base.isJust e_mb 
		then (Just class_name, Optic.fill at Nothing c)
		else (Nothing, c)
	in Base.either (const (Nothing, c)) when_there (Optic.match at c)

move_class_out_from_elem :: ClassName -> Elem Text e -> (Maybe ClassName, Elem Text e)
move_class_out_from_elem class_name =
	let
		traversal =
			Category2.identity
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

over_Elem' :: Configuration -> Optic.Iso' (Elem Text TextSplit.H) (Elem Text Text)
over_Elem' config =
	let
		parse :: Elem Text TextSplit.H -> Elem Text Text
		parse elem =
			case ofElem_core elem of
				(cs, text) ->
					Optic.fn_up inElem_labels
						(Intermediate.add_new_classes_to_Labels cs) (map snd elem)
		in Optic.Iso (move_classes_out_from_elem' config) parse

layer :: Configuration -> Optic.Iso' ElemTT ElemTT
layer config = Optic.lift_iso (TextSplit.layer config) >**> over_Elem' config
