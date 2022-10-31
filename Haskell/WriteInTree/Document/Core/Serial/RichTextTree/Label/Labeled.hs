module WriteInTree.Document.Core.Serial.RichTextTree.Label.Labeled
(
	Structure.add_new_classes_to_Labels,
	ofElem_pos,
	ofElem_class_values,
	ofElem_address,
	ofElem_classes,
	elem_has_class,
	Labeled (..),
)
where

import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..))
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Position, Positioned (..))
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (Labels)

import qualified Data.Foldable as Fold
import qualified Fana.Data.Key.Traversable as TravKey
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure as Structure
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Char = Base.Char
type Text = [Char]


type Labeled e = (Labels, Positioned e)


ofElem_pos :: Optic.Lens' Position (Labeled e)
ofElem_pos = Cat2.identity >**>^ Pos.inPositioned_position >**>^ Optic.lens_2

ofElem_address :: Labeled e -> Maybe Text
ofElem_address = fst >>> Structure.address_of_Labels >>> map unwrapPageAddress

ofElem_class_values :: Labeled e -> [Text]
ofElem_class_values =
	id
	>>> fst
	>>> Structure.classes_of_Labels
	>>> map TravKey.keys
	>>> Fold.concat

ofElem_classes :: Optic.AffineTraversal' Structure.ClassesMap (Labeled e)
ofElem_classes = Cat2.identity >**>^ Optic.prism_Maybe >**>^ Structure.ofLabels_classes >**>^ Optic.lens_1

elem_has_class :: Text -> Labeled e -> Bool
elem_has_class class_text = fst >>> Structure.labels_has_class class_text
