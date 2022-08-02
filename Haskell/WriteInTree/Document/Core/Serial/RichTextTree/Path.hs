module WriteInTree.Document.Core.Serial.RichTextTree.Path
(
	ElemHP (..), ElemHPT,
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

data ElemHP e = ElemHP
	{ inElemHPPos :: Pos.Position
	, inElemHPCore :: Tt.Elem e
	}
	deriving (Eq, Functor, Foldable, Traversable)
type ElemHPT = ElemHP Text

instance Pos.HasPosition (ElemHP e) where get_position = inElemHPPos
instance Default e => Default (ElemHP e) where def = ElemHP def def

parse :: Tree ElemLT -> Tree ElemHPT
parse = Tree.with_path_to_trunk >>> map (Bifunctor.first (map (Tree.rootLabel >>> Tt.elemValue)) >>> uncurry ElemHP)

layer :: Optic.Iso' (Tree ElemLT) (Tree ElemHPT)
layer = Optic.Iso (map inElemHPCore) parse
