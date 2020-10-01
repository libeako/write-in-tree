module WriteInTree.Document.Core.Serial.RichTextTree.Label.Elem
(
	Intermediate.Id (..),
	Intermediate.add_new_classes_to_Labels,
	Intermediate.inLabel_id_source_mb,
	Intermediate.id_of_Labels,
	fromElem_id_au_content,
	ofElem_ordinal,	ofElem_pos,
	ofElem_source_path,
	ofElem_class_values,
	ofElem_id_u_content,
	inElem_idu, inElem_labels,
	ofElem_classes,
	elem_has_class,
	Elem (..), ElemT,
	elem_pd, elem_dp,
)
where

import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude

import qualified Data.Foldable as Fold
import qualified Fana.Data.HasSingle as Fana
import qualified Fana.Data.Key.Traversable as TravKey
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Categories.AffineTraverse as Optic
import qualified Fana.Optic.Concrete.Categories.Lens as Optic
import qualified Fana.Optic.Concrete.Categories.Prism as Optic
import qualified Fana.Optic.Concrete.Categories.Traversal as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Comment as Comment
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.TextStructure as Ts
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Intermediate as Intermediate
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Ord as Ord
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Char = Base.Char
type Text = [Char]
type ElemP = Comment.ElemD
type Source = ElemP ()
type ElemPT = ElemP Text


data Elem id e = Elem
	{ ofElem_auto_id :: Maybe Text
		-- ^ automatic identifier
	, ofElem_position :: Pos.PositionFields
	, ofElem_comment_children :: [Comment.CommentElem]
	, ofElem_labels :: Intermediate.Labels id
	, ofElem_core :: e
	} deriving (Functor, Foldable, Traversable)
type ElemT = Elem Text (Ts.Content')

instance Fana.HasSingle (Elem id) where elem = ofElem_core

ofElem_pos :: Optic.Lens' Pos.PositionFields (Elem id e)
ofElem_pos = Optic.lens_from_get_set ofElem_position (\ e c -> c { ofElem_position = e })

ofElem_ordinal :: Optic.Lens' Ord.Ordinal (Elem id e)
ofElem_ordinal = Pos.ofPositionFields_ordinal >**> ofElem_pos

ofElem_source_path :: Elem id e -> Pos.Position
ofElem_source_path = ofElem_position >>> Pos.field_source_path

ofElem_id_u_content :: Elem id e -> Maybe id
ofElem_id_u_content = ofElem_labels >>> Intermediate.id_of_Labels >>> map Intermediate.valueId

fromElem_id_au_content :: Elem id e -> (Maybe Text, Maybe id)
fromElem_id_au_content = liftA2 (,) ofElem_auto_id ofElem_id_u_content

ofElem_class_values :: Elem id e -> [Text]
ofElem_class_values = id 
	>>> ofElem_labels 
	>>> Intermediate.classes_of_Labels 
	>>> map (Intermediate.classes >>> TravKey.keys) 
	>>> Fold.concat

inElem_labels :: Optic.Lens (Intermediate.Labels id_1) (Intermediate.Labels id_2) (Elem id_1 e) (Elem id_2 e)
inElem_labels = Optic.lens_from_get_set ofElem_labels (\ p w -> w { ofElem_labels = p })

inElem_idu :: Optic.Traversal (id_1) (id_2) (Elem id_1 e) (Elem id_2 e)
inElem_idu = Intermediate.inLabels_id >**>^ inElem_labels

ofElem_classes :: Optic.AffineTraversal' Intermediate.Classes (Elem id e)
ofElem_classes = Category2.empty >**>^ Optic.prism_Maybe >**>^ Intermediate.ofLabels_classes >**>^ inElem_labels

elem_has_class :: Text -> Elem id e -> Bool
elem_has_class class_text = ofElem_labels >>> Intermediate.labels_has_class class_text


instance Pos.HasPosition (Elem id e) where get_position = ofElem_source_path

-- | convert an element from data to picture format.
elem_dp :: Elem id e -> ElemP e
elem_dp x = Comment.ElemD
	{ Comment.elemId = ofElem_auto_id x
	, Comment.elemPosition = ofElem_position x
	, Comment.elemCommentChildren = ofElem_comment_children x
	, Comment.elemValue = ofElem_core x
	}
-- | convert an element from picture to data format.
elem_pd :: Intermediate.Labels id -> ElemP e -> Elem id e
elem_pd labels p = Elem
	{ ofElem_auto_id = Comment.elemId p
	, ofElem_position = Comment.elemPosition p
	, ofElem_comment_children = Comment.elemCommentChildren p
	, ofElem_labels = labels
	, ofElem_core = Comment.elemValue p
	}
