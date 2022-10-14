module WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure
(
	PageAddress (..),
	ClassesMap,
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


type Char = Base.Char
type Text = [Char]

type ClassesMap = StringyMap.Map Char ()

data Any = IntermId Text | IntermClass ClassesMap


index_classes :: [Text] -> Either (Accu.Accumulated Text) ClassesMap
index_classes = let
	error_message :: Text -> Accu.Accumulated Text
	error_message text = "multiple instances of class \"" <> Accu.single text <> "\""
	in map (Pair.before ()) >>> MapI.from_list_of_uniques >>> Bifunctor.first (fst >>> error_message)

add_new :: [Text] -> ClassesMap -> ClassesMap
add_new incoming_classes old_classes = let
	updated_classes :: ClassesMap
	updated_classes = let
		add_class :: Text -> Fn.Endo ClassesMap
		add_class c = LensAt.ensure_existence_at c ()
		in Fold.foldr' add_class old_classes incoming_classes
	in updated_classes

contains :: Text -> ClassesMap -> Bool
contains class_text = LensAt.get_at class_text >>> Base.isJust

data PageAddress = 
	PageAddress { unwrapPageAddress :: Text }
	deriving Eq

-- | user given labels of a node.
data Labels id = Labels
	{ id_of_Labels :: Maybe id
	, address_of_Labels :: Maybe PageAddress
	, classes_of_Labels :: Maybe ClassesMap
	}
	deriving (Eq, Functor, Foldable, Traversable)
type LabelsT = Labels Text

no_Labels :: Labels id
no_Labels = Labels Nothing Nothing Nothing

instance Default (Labels id) where def = no_Labels

inLabels_id :: Optic.Traversal e1 e2 (Labels e1) (Labels e2)
inLabels_id = Optic.from_Traversable

inLabel_id_source_mb :: 
	Optic.Lens
		(Maybe id_1) (Maybe id_2)
		(Labels id_1) (Labels id_2)
inLabel_id_source_mb = Optic.lens_from_get_set id_of_Labels (\ p w -> w { id_of_Labels = p })

ofLabels_classes :: Optic.Lens' (Maybe ClassesMap) (Labels id)
ofLabels_classes = Optic.lens_from_get_set classes_of_Labels (\ e c -> c { classes_of_Labels = e })

add_new_classes_to_Labels :: [Text] -> Fn.Endo (Labels id)
add_new_classes_to_Labels additional = let
	new_imc :: Fn.Endo (Maybe ClassesMap)
	new_imc = let
		real_addition :: Fn.Endo (Maybe ClassesMap)
		real_addition = Base.fromMaybe def >>> (add_new additional) >>> Just
		in if List.null additional then id else real_addition
	in Optic.fn_up ofLabels_classes new_imc

labels_has_class :: Text -> Labels id -> Bool
labels_has_class class_text = classes_of_Labels >>> Base.maybe False (contains class_text)
