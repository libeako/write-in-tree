module WriteInTree.Document.Core.Serial.RichTextTree.Path
(
	ElemH (..), ElemHT,
	layer,
)
where

import Data.Default.Class
import Data.Tree (Tree)
import Fana.Prelude

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
-}

type ElemL e = Tt.Elem e
type ElemLT = ElemL Text

data ElemH e = ElemH
	{ inElemPos :: Pos.Position
	, inElemCore :: Tt.Elem e
	}
	deriving (Eq, Functor, Foldable, Traversable)
type ElemHT = ElemH Text

instance Pos.HasPosition (ElemH e) where get_position = inElemPos
instance Default e => Default (ElemH e) where def = ElemH def def

parse :: Tree ElemLT -> Tree ElemHT
parse = Tree.with_path_to_trunk >>> map (Bifunctor.first (map (Tree.rootLabel >>> Tt.elemValue)) >>> uncurry ElemH)

layer :: Optic.Iso' (Tree ElemLT) (Tree ElemHT)
layer = Optic.Iso (map inElemCore) parse
