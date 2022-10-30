module WriteInTree.Document.Core.Serial.RichTextTree.Path
(
	ElemHR, ElemHRT, ElemHP (..), ElemHPT,
	layer,
)
where

import Data.Tree (Tree)
import Fana.Prelude

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Tree as Tree
import qualified Fana.Data.HasSingle as Fana
import qualified Fana.Data.Tree.OfBase as Tree
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified Technical.TextTree.Data as Tt
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String

type ElemL e = Tt.Elem e
type ElemLT = ElemL Text
type ElemHR e = Tt.Elem e
type ElemHRT = ElemHR Text
data ElemHP e =
	ElemHP
	{ inElemHPPos :: Pos.Position
	, inElemHPCore :: Tt.Elem e
	}
	deriving (Eq, Functor, Foldable, Traversable)
type ElemHPT = ElemHP Text

instance Pos.HasPosition (ElemHP e) where get_position = inElemHPPos
instance Fana.HasSingle ElemHP where elem = inElemHPCore >>> Tt.elemValue

parse :: Tree ElemLT -> Tree ElemHPT
parse = Tree.with_path_to_trunk >>> map (Bifunctor.first (map (Tree.rootLabel >>> Tt.elemValue)) >>> uncurry ElemHP)

layer :: Optic.Iso (Tree ElemLT) (Tree ElemLT) (Tree ElemHRT) (Tree ElemHPT)
layer = Optic.Iso id parse
