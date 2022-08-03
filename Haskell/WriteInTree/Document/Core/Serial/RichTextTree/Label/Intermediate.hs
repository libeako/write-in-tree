module WriteInTree.Document.Core.Serial.RichTextTree.Label.Intermediate
(
	ClassesMap, Classes (..),
	ofClasses_classes,
	Any (..),
	Labels (..), LabelsT, no_Labels,
	inLabels_id, inLabel_id_source_mb, ofLabels_classes, labels_has_class, add_new_classes_to_Labels,
	index_classes,
	contains,
	add_new,
)
where

import Data.Default.Class
import Fana.Prelude

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Maybe as Base
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Data.Function as Fn
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.Key.LensToMaybeElement as LensAt
import qualified Fana.Data.Key.Map.Interface as MapI
import qualified Fana.Data.Key.Map.KeyIsString as StringyMap
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Path as Path


type Char = Base.Char
type Text = [Char]
type ElemP = Path.ElemHP
type Source = ElemP ()

type ClassesMap = StringyMap.Map Char ()

data Classes = Classes
	{ source_of_classes_trunk :: Maybe Source
	, classes :: ClassesMap
	}
	deriving (Eq)

ofClasses_classes :: Optic.Lens' ClassesMap Classes
ofClasses_classes = Optic.lens_from_get_set classes (\ e c -> c { classes = e })

instance Default Classes where def = Classes def def

data Any = IntermId Text | IntermClass Classes


index_classes :: [Text] -> Either (Accu.Accumulated Text) ClassesMap
index_classes = let
	error_message :: Text -> Accu.Accumulated Text
	error_message text = "multiple instances of class \"" <> Accu.single text <> "\""
	in map (Pair.before ()) >>> MapI.from_list_of_uniques >>> Bifunctor.first (fst >>> error_message)

add_new :: [Text] -> Classes -> Classes
add_new incoming_classes (Classes trunk_source old_classes) = let
	updated_classes :: ClassesMap
	updated_classes = let
		updated_old_classes :: ClassesMap
		updated_old_classes = old_classes
		add_class :: Text -> Fn.Endo ClassesMap
		add_class c = LensAt.ensure_existence_at c ()
		in Fold.foldr' add_class updated_old_classes incoming_classes
	in (Classes trunk_source updated_classes)

contains :: Text -> Classes -> Bool
contains class_text = classes >>> LensAt.get_at class_text >>> Base.isJust

-- | user given labels of a node.
data Labels id = Labels
	{ id_of_Labels :: Maybe id
	, classes_of_Labels :: Maybe Classes
	}
	deriving (Eq, Functor, Foldable, Traversable)
type LabelsT = Labels Text

no_Labels :: Labels id
no_Labels = Labels Nothing Nothing

inLabels_id :: Optic.Traversal e1 e2 (Labels e1) (Labels e2)
inLabels_id = Optic.from_Traversable

inLabel_id_source_mb :: 
	Optic.Lens
		(Maybe id_1) (Maybe id_2)
		(Labels id_1) (Labels id_2)
inLabel_id_source_mb = Optic.lens_from_get_set id_of_Labels (\ p w -> w { id_of_Labels = p })

ofLabels_classes :: Optic.Lens' (Maybe Classes) (Labels id)
ofLabels_classes = Optic.lens_from_get_set classes_of_Labels (\ e c -> c { classes_of_Labels = e })

add_new_classes_to_Labels :: [Text] -> Fn.Endo (Labels id)
add_new_classes_to_Labels additional = let
	new_imc :: Fn.Endo (Maybe Classes)
	new_imc = let
		real_addition :: Fn.Endo (Maybe Classes)
		real_addition = Base.fromMaybe def >>> (add_new additional) >>> Just
		in if List.null additional then id else real_addition
	in Optic.fn_up ofLabels_classes new_imc

labels_has_class :: Text -> Labels id -> Bool
labels_has_class class_text = classes_of_Labels >>> Base.maybe False (contains class_text)
