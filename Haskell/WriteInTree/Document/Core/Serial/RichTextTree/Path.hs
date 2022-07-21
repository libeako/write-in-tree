module WriteInTree.Document.Core.Serial.RichTextTree.Path
(
	DataElemO, DataElemOT,
	layer,
	lift_serialization_layer,
	
	CommentElemD (..), CommentElemDT,
	comment_layer,
)
where

import Data.Default.Class
import Data.Tree (Tree)
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Position

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Tree as Tree
import qualified Fana.Data.Tree.OfBase as Tree
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified Technical.TextTree.Data as Tt
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String


-- the input and output types here may have inner ["i"] and outer ["o"] versions; 
-- the inner versions are more convenient for the inner working of this layer

-- | the real form of the picture element data type, as this layer gets it.
type PictureElemO e = Tt.Elem e
-- | the form of the picture element data type, as this layer prefers it.
type PictureElemI e = (Text, Tt.Elem e)
type PictureElemOT = PictureElemO Text
type PictureElemIT = PictureElemI Text

picture_oi :: PictureElemOT -> PictureElemIT
picture_oi elem = (Tt.elemValue elem, elem)

picture_io :: PictureElemI e -> PictureElemO e
picture_io (_, elem) = elem

picture_elem_iso :: Optic.Iso' PictureElemOT PictureElemIT
picture_elem_iso = Optic.Iso picture_io picture_oi

picture_iso :: Optic.Iso' (Tree PictureElemOT) (Tree PictureElemIT)
picture_iso = Optic.iso_up picture_elem_iso

type DataElemO e = ([Text], Tt.Elem e)
type DataElemI e = ([PictureElemI e], PictureElemI e)
type DataElemOT = DataElemO Text
type DataElemIT = DataElemI Text

tree_up :: Tree e -> Tree ([e], e)
tree_up = Tree.with_path_to_trunk >>> map (Bifunctor.first (map Tree.rootLabel))

data_io :: DataElemI e -> DataElemO e
data_io (path, (pos, elem)) = (map fst path, elem)

data_oi_2 :: Tt.Elem' -> PictureElemIT
data_oi_2 elem = (Tt.elemValue elem, elem)

up :: Tree (PictureElemI e) -> Tree (DataElemO e)
up = tree_up >>> map data_io

down :: Tree DataElemOT -> Tree PictureElemIT
down = map (snd >>> data_oi_2)

core_iso :: Optic.Iso' (Tree PictureElemIT) (Tree DataElemOT)
core_iso = Optic.Iso down up

layer :: Optic.Iso' (Tree PictureElemOT) (Tree DataElemOT)
layer = picture_iso >**> core_iso

lift_serialization_layer :: 
	forall c e pr pp dr dp .
	(Traversable c, forall l . HasPosition (c l)) =>
	Optic.PartialIso e pr pp dr dp -> Optic.PartialIso (Positioned e) (c pr) (c pp) (c dr) (c dp)
lift_serialization_layer (Optic.PartialIso render parse) =
	let
		new_parse :: c pp -> Either (Positioned e) (c dp)
		new_parse c = Bifunctor.first (Positioned (get_position c)) (traverse parse c)
		in Optic.PartialIso (map render) new_parse


-- | meaningful [not comment] element type at the data level.
data CommentElemD e = CommentElemD
	{ elemId :: Maybe Text
	, elemPosition :: Pos.Position
	, elemValue :: e
	}
	deriving (Eq, Functor, Foldable, Traversable)
type CommentElemDT = CommentElemD Text

instance Pos.HasPosition (CommentElemD e) where get_position = elemPosition
instance Default e => Default (CommentElemD e) where def = CommentElemD def def def


comment_elem_pd :: DataElemO Text -> CommentElemDT
comment_elem_pd (pos, (Tt.Elem identifier text)) = CommentElemD
	{ elemId = identifier
	, elemPosition = pos
	, elemValue = text 
	}
comment_elem_dp :: CommentElemDT -> DataElemO Text
comment_elem_dp e = (Pos.get_position e, Tt.Elem (elemId e) (elemValue e))

comment_layer :: Optic.Iso' (Tree (DataElemO Text)) (Tree CommentElemDT)
comment_layer = Optic.Iso (map comment_elem_dp) (map comment_elem_pd)
