module WriteInTree.Document.Core.Serial.RichTextTree.Label.Intermediate
(
	Id (..), IdT, 
	Class (..), ClassesMap, Classes (..),
	ofClasses_classes,
	Any (..),
	Labels (..), LabelsT,
	inLabels_id, inLabel_id_source_mb, ofLabels_classes, labels_has_class, add_new_classes_to_Labels,
	index_classes,
	contains,
	add_new,
)
where

import Data.Default.Class
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Maybe as Base
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Data.Function as Fn
import qualified Fana.Data.HasSingle as Fana
import qualified Fana.Data.Key.LensToMaybeElement as LensAt
import qualified Fana.Data.Key.Map.Interface as MapI
import qualified Fana.Data.Key.Map.KeyIsString as StringyMap
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Comment as Comment
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Ord as Ord
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Char = Base.Char
type Text = [Char]
type ElemP = Comment.ElemD
type Source = ElemP ()

data Id e = Id
	{ source_of_id_trunk :: ElemP ()
	, source_of_id_value :: ElemP ()
	, valueId :: e
	}
	deriving (Eq, Functor, Foldable, Traversable)
instance Fana.HasSingle Id where elem = valueId

type IdT = Id Text

id_content :: Optic.Lens e1 e2 (Id e1) (Id e2)
id_content = Optic.lens_from_get_set valueId (\ p w -> w { valueId = p })

type ClassesMap = StringyMap.Map Char Source

data Class = Class { classSource :: ElemP (), classValue :: Text }
data Classes = Classes
	{ source_of_classes_trunk :: Maybe Source
	, classes :: ClassesMap
	}
	deriving (Eq)

ofClasses_classes :: Optic.Lens' ClassesMap Classes
ofClasses_classes = Optic.lens_from_get_set classes (\ e c -> c { classes = e })

instance Default Classes where def = Classes def def

data Any = IntermId (Id Text) | IntermClass Classes


index_classes :: [Class] -> Either (Accu.Accumulated Text) ClassesMap
index_classes = let
	to_pair :: Class -> (Text, Source)
	to_pair (Class s t) = (t, s)
	error_message :: Text -> Accu.Accumulated Text
	error_message text = "multiple instances of class \"" <> Accu.single text <> "\""
	in map to_pair >>> MapI.from_list_of_uniques >>> Bifunctor.first error_message

add_new :: [Text] -> Classes -> Classes
add_new incoming_classes_names (Classes trunk_source old_classes) = let
	updated_classes :: ClassesMap
	updated_classes = let
		updated_old_classes :: ClassesMap
		updated_old_classes = let
			lens_ordinal :: Optic.Lens' Ord.Ordinal Source
			lens_ordinal = Category2.empty >**>^ Pos.ofPositionFields_ordinal >**>^ Comment.ofElem_position
			in map (Optic.fn_up lens_ordinal (() : )) old_classes
		source_of_new_class :: Ord.SimpleOrdinal -> Text -> Source
		source_of_new_class simple_ordinal name = let
			ordinal = [(), simple_ordinal]
			in Comment.ElemD 
				Nothing 
				(
					Pos.PositionFields ordinal 
						(
							Pos.PositionAtLevel ordinal name : 
							Fold.concat (Fold.toList (map Pos.get_position trunk_source))
						)
				)
				()
		add_class :: (Ord.SimpleOrdinal, Text) -> Fn.Endo ClassesMap
		add_class c = LensAt.ensure_existence_at (snd c) (uncurry source_of_new_class c)
		incoming_classes = List.zip (List.repeat ()) incoming_classes_names
		in Fold.foldr' add_class updated_old_classes incoming_classes
	in (Classes trunk_source updated_classes)

contains :: Text -> Classes -> Bool
contains class_text = classes >>> LensAt.get_at class_text >>> Base.isJust

-- | user given labels of a node.
data Labels id = Labels
	{ id_of_Labels :: Maybe (Id id)
	, classes_of_Labels :: Maybe Classes
	}
	deriving (Eq, Functor, Foldable, Traversable)
type LabelsT = Labels Text

inLabels_id :: Optic.Traversal e1 e2 (Labels e1) (Labels e2)
inLabels_id = Optic.from_Traversable

inLabel_id_source_mb :: 
	Optic.Lens
		(Maybe (Id id_1)) (Maybe (Id id_2))
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
