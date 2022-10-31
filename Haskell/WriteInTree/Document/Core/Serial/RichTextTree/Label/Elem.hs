module WriteInTree.Document.Core.Serial.RichTextTree.Label.Elem
(
	Structure.add_new_classes_to_Labels,
	ofElem_pos,
	ofElem_class_values,
	ofElem_address,
	inElem_labels,
	ofElem_classes,
	elem_has_class,
	default_Elem_context,
	Elem (..),
	elem_pd, elem_dp,
)
where

import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..))
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Position, Positioned (..), get_position)
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (Labels)

import qualified Data.Foldable as Fold
import qualified Fana.Data.HasSingle as Fana
import qualified Fana.Data.Key.Traversable as TravKey
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure as Structure
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Char = Base.Char
type Text = [Char]


data Elem e =
	Elem
	{ ofElem_labels :: Labels
	, ofElem_core :: Positioned e
	}
	deriving (Eq, Functor, Foldable, Traversable)

instance Fana.HasSingle Elem where elem = ofElem_core >>> positionedValue

inElem_labels :: Optic.Lens Structure.Labels Structure.Labels (Elem e) (Elem e)
inElem_labels = Optic.lens_from_get_set ofElem_labels (\ p w -> w { ofElem_labels = p })

inElem_core :: Optic.Lens (Positioned e1) (Positioned e2) (Elem e1) (Elem e2)
inElem_core = Optic.lens_from_get_set ofElem_core (\ p w -> w { ofElem_core = p })

ofElem_pos :: Optic.Lens' Position (Elem e)
ofElem_pos = Cat2.identity >**>^ Pos.inPositioned_position >**>^ inElem_core

ofElem_address :: Elem e -> Maybe Text
ofElem_address = ofElem_labels >>> Structure.address_of_Labels >>> map unwrapPageAddress

ofElem_class_values :: Elem e -> [Text]
ofElem_class_values =
	id 
	>>> ofElem_labels 
	>>> Structure.classes_of_Labels
	>>> map TravKey.keys
	>>> Fold.concat


ofElem_classes :: Optic.AffineTraversal' Structure.ClassesMap (Elem e)
ofElem_classes = Cat2.identity >**>^ Optic.prism_Maybe >**>^ Structure.ofLabels_classes >**>^ inElem_labels

elem_has_class :: Text -> Elem e -> Bool
elem_has_class class_text = ofElem_labels >>> Structure.labels_has_class class_text


instance Pos.HasPosition (Elem e) where get_position = ofElem_core >>> get_position

default_Elem_context :: e -> Elem e
default_Elem_context e = Elem def (Positioned def e)

-- | convert an element from data to picture format.
elem_dp :: Elem e -> Positioned e
elem_dp = ofElem_core

-- | convert an element from picture to data format.
elem_pd :: Structure.Labels -> Positioned e -> Elem e
elem_pd labels p =
	Elem
	{ ofElem_labels = labels
	, ofElem_core = p
	}
