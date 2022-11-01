module WriteInTree.Document.Core.Serial.RichTextTree.Label.InlineClassCoding
(
	layer,
)
where

import Data.Functor.Identity (Identity (..))
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (Labels)
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (..))
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
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure as Structure
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.TextSplit as TextSplit


type Char = Base.Char
type Text = [Char]
type LabeledPositioned e = (Labels, Positioned e)
type LabeledT = LabeledPositioned Text


move_class_out_from_container :: forall e c . ClassName -> Optic.AffineTraversal' (Maybe e) c -> c -> (Maybe ClassName, c)
move_class_out_from_container class_name at c = let
	when_there :: Maybe e -> (Maybe Text, c)
	when_there e_mb = if Base.isJust e_mb 
		then (Just class_name, Optic.fill at Nothing c)
		else (Nothing, c)
	in Base.either (const (Nothing, c)) when_there (Optic.match at c)

move_class_out_from_elem :: ClassName -> LabeledPositioned e -> (Maybe ClassName, LabeledPositioned e)
move_class_out_from_elem class_name =
	let
		traversal =
			Category2.identity
			>**>^ Map.lens_at class_name 
			>**>^ Optic.prism_Maybe 
			>**>^ Structure.ofLabels_classes
			>**>^ Optic.lens_1
		in move_class_out_from_container class_name traversal

move_class_out_from_elem_st :: ClassName -> Mtl.State (LabeledPositioned e) (Maybe ClassName)
move_class_out_from_elem_st = move_class_out_from_elem >>> map Identity >>> Mtl.StateT

move_classes_out_from_elem_st :: forall e . Configuration -> Mtl.State (LabeledPositioned e) [Maybe ClassName]
move_classes_out_from_elem_st = traverse (ilc_name >>> move_class_out_from_elem_st)

move_classes_out_from_elem :: forall e . Configuration -> LabeledPositioned e -> ([ClassName], LabeledPositioned e)
move_classes_out_from_elem = 
	id
	>>> move_classes_out_from_elem_st 
	>>> Mtl.runState
	>>> map (Bifunctor.first Base.catMaybes)

move_classes_out_from_elem' :: Configuration -> LabeledPositioned Text -> LabeledPositioned TextSplit.H
move_classes_out_from_elem' config =
	move_classes_out_from_elem config >>>
	(\ (classes, elem) -> (map >>> map) (Pair.after classes) elem)

over_Labeled' :: Configuration -> Optic.Iso' (LabeledPositioned TextSplit.H) (LabeledPositioned Text)
over_Labeled' config =
	let
		parse :: LabeledPositioned TextSplit.H -> LabeledPositioned Text
		parse elem =
			case positionedValue (snd elem) of
				(cs, text) ->
					Optic.fn_up Optic.lens_1
						(Structure.add_new_classes_to_Labels cs) ((map >>> map) snd elem)
		in Optic.Iso (move_classes_out_from_elem' config) parse

layer :: Configuration -> Optic.Iso' LabeledT LabeledT
layer config = (Optic.lift_iso >>> Optic.lift_iso) (TextSplit.layer config) >**> over_Labeled' config
