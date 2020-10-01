module Technical.Xml.Data
(
	Text,
	Attribute, Head (..), Content, Element,
	element_as_content,
	text, tree,
	Labels (..), render_labels, melt_labels_in_content, melt_labels_in_element,
	lens_id_of_Element, lens_classes_of_Element,
	ContentL, ElementL,
)
where

import Data.Default.Class (Default (..))
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude

import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Fana.Data.Function as Fn
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.Tree.Discriminating as DTree
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base


type Text = Base.String


-- | . name value pair
type Attribute = (Text, Text)
data Head e = Head { head_name :: Text, head_attributes :: [Attribute], head_else :: e }
type Content e = DTree.Tree [] Text (Head e) ()
type Element e = (Head e, [Content e])


-- * constructors


text :: Text -> Content e
text t = DTree.leaf () t

tree :: Head e -> [Element e] -> Element e
tree head cs = (head, map element_as_content cs)


-- * optics

lens_head_attributes :: Optic.Lens' [Attribute] (Head e)
lens_head_attributes = Optic.lens_from_get_set head_attributes (\ e c -> c { head_attributes = e })

lens_head_else :: Optic.Lens e1 e2 (Head e1) (Head e2)
lens_head_else = Optic.lens_from_get_set head_else (\ e c -> c { head_else = e })


-- * else

element_as_content :: Element e -> Content e
element_as_content (h, c) = DTree.joint () h c


-- * labels

data Labels = Labels { label_id :: Maybe Text, label_classes :: [Text] }

instance Default Labels where def = Labels def def

lens_id_of_Labels :: Optic.Lens' (Maybe Text) Labels
lens_id_of_Labels = Optic.lens_from_get_set label_id (\ e c -> c { label_id = e })

lens_classes_of_Labels :: Optic.Lens' [Text] Labels
lens_classes_of_Labels = Optic.lens_from_get_set label_classes (\ e c -> c { label_classes = e })

lens_id_of_Element :: Optic.Lens' (Maybe Text) ElementL
lens_id_of_Element = Category2.empty >**>^ lens_id_of_Labels >**>^ lens_head_else >**>^ Optic.lens_1

lens_classes_of_Element :: Optic.Lens' [Text] ElementL
lens_classes_of_Element = Category2.empty >**>^ lens_classes_of_Labels >**>^ lens_head_else >**>^ Optic.lens_1

type ContentL = Content Labels
type ElementL = Element Labels

render_id :: Text -> Attribute
render_id = Pair.after "id"

render_classes :: [Text] -> Maybe Attribute
render_classes class_names = 
	if List.null class_names then Nothing 
		else Just ("class", Fold.concat (List.intersperse " " class_names))

render_labels :: Labels -> [Attribute]
render_labels ls = Fold.foldMap Fold.toList [map render_id (label_id ls), render_classes (label_classes ls)]

melt_labels_to_attributes :: Labels -> Fn.Endo [Attribute]
melt_labels_to_attributes ls = (render_labels ls <>)

melt_labels_in_head :: Head Labels -> Head ()
melt_labels_in_head head = 
	let
		labels = head_else head
		clear_labels = Optic.fill lens_head_else ()
		add_new_attributes = Optic.fn_up lens_head_attributes (melt_labels_to_attributes labels)
		in (clear_labels >>> add_new_attributes) head

melt_labels_in_content :: Content Labels -> Content ()
melt_labels_in_content = DTree.map_all id (map melt_labels_in_head)

melt_labels_in_element :: Element Labels -> Element ()
melt_labels_in_element (h, c) = (melt_labels_in_head h, map melt_labels_in_content c)
