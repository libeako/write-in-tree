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
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (Positioned))

import qualified Data.Foldable as Fold
import qualified Fana.Data.HasSingle as Fana
import qualified Fana.Data.Key.Traversable as TravKey
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Categories.AffineTraverse as Optic
import qualified Fana.Optic.Concrete.Categories.Lens as Optic
import qualified Fana.Optic.Concrete.Categories.Prism as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure as Structure
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Char = Base.Char
type Text = [Char]
type ElemP = Positioned
type Source = ElemP ()
type ElemPT = ElemP Text


data Elem e = Elem
	{ ofElem_position :: Pos.Position
	, ofElem_labels :: Structure.Labels
	, ofElem_core :: e
	}
	deriving (Eq, Functor, Foldable, Traversable)

instance Fana.HasSingle Elem where elem = ofElem_core

ofElem_pos :: Optic.Lens' Pos.Position (Elem e)
ofElem_pos = Optic.lens_from_get_set ofElem_position (\ e c -> c { ofElem_position = e })

ofElem_address :: Elem e -> Maybe Text
ofElem_address = ofElem_labels >>> Structure.address_of_Labels >>> map unwrapPageAddress

ofElem_class_values :: Elem e -> [Text]
ofElem_class_values =
	id 
	>>> ofElem_labels 
	>>> Structure.classes_of_Labels
	>>> map TravKey.keys
	>>> Fold.concat

inElem_labels :: Optic.Lens Structure.Labels Structure.Labels (Elem e) (Elem e)
inElem_labels = Optic.lens_from_get_set ofElem_labels (\ p w -> w { ofElem_labels = p })

ofElem_classes :: Optic.AffineTraversal' Structure.ClassesMap (Elem e)
ofElem_classes = Category2.identity >**>^ Optic.prism_Maybe >**>^ Structure.ofLabels_classes >**>^ inElem_labels

elem_has_class :: Text -> Elem e -> Bool
elem_has_class class_text = ofElem_labels >>> Structure.labels_has_class class_text


instance Pos.HasPosition (Elem e) where get_position = ofElem_position

default_Elem_context :: e -> Elem e
default_Elem_context e = Elem def def e

-- | convert an element from data to picture format.
elem_dp :: Elem e -> ElemP e
elem_dp x = Positioned (ofElem_position x) (ofElem_core x)

-- | convert an element from picture to data format.
elem_pd :: Structure.Labels -> ElemP e -> Elem e
elem_pd labels p =
	Elem
	{ofElem_position = Pos.position p
	, ofElem_labels = labels
	, ofElem_core = Pos.positionedValue p
	}
