module WriteInTree.Document.Core.Serial.RichTextTree.Path
(
	ElemHE (..), ElemHET,
	layer,
	lift_serialization_layer,
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

{-
	The 2 sides of this layer:
		"h" = high level
		"l" = low  level
	
	The 2 layers of any of the sides:
		"e" = external
		"i" = internal
-}


type ElemLE e = Tt.Elem e
type ElemLI e = (Text, Tt.Elem e)
type ElemLET = ElemLE Text
type ElemLIT = ElemLI Text

l_ei :: ElemLET -> ElemLIT
l_ei elem = (Tt.elemValue elem, elem)

l_ie :: ElemLI e -> ElemLE e
l_ie (_, elem) = elem

l_elem_iso :: Optic.Iso' ElemLET ElemLIT
l_elem_iso = Optic.Iso l_ie l_ei

l_tree_iso :: Optic.Iso' (Tree ElemLET) (Tree ElemLIT)
l_tree_iso = Optic.iso_up l_elem_iso


data ElemHE e = ElemHE
	{ elemPosition :: Pos.Position
	, commentTtElem :: Tt.Elem e
	}
	deriving (Eq, Functor, Foldable, Traversable)
type ElemHET = ElemHE Text

instance Pos.HasPosition (ElemHE e) where get_position = elemPosition
instance Default e => Default (ElemHE e) where def = ElemHE def def

type ElemHI e = ([ElemLI e], ElemLI e)
type ElemHIT = ElemHI Text

tree_up :: Tree e -> Tree ([e], e)
tree_up = Tree.with_path_to_trunk >>> map (Bifunctor.first (map Tree.rootLabel))

h_io :: ElemHI e -> ElemHE e
h_io (path, (pos, elem)) = ElemHE (map fst path) elem

tt_li :: Tt.Elem' -> ElemLIT
tt_li elem = (Tt.elemValue elem, elem)

up :: Tree (ElemLI e) -> Tree (ElemHE e)
up = tree_up >>> map h_io

down :: Tree ElemHET -> Tree ElemLIT
down = map (commentTtElem >>> tt_li)

core_iso :: Optic.Iso' (Tree ElemLIT) (Tree ElemHET)
core_iso = Optic.Iso down up

layer :: Optic.Iso' (Tree ElemLET) (Tree ElemHET)
layer = l_tree_iso >**> core_iso

lift_serialization_layer ::
	forall c e pr pp dr dp .
	(Traversable c, forall l . HasPosition (c l)) =>
	Optic.PartialIso e pr pp dr dp -> Optic.PartialIso (Positioned e) (c pr) (c pp) (c dr) (c dp)
lift_serialization_layer (Optic.PartialIso render parse) =
	let
		new_parse :: c pp -> Either (Positioned e) (c dp)
		new_parse c = Bifunctor.first (Positioned (get_position c)) (traverse parse c)
		in Optic.PartialIso (map render) new_parse
