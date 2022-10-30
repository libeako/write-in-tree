module WriteInTree.Document.Core.Serial.RichTextTree.Path
(
	ElemHR, ElemHRT, ElemHP (..), ElemHPT,
	layer,
)
where

import Data.Tree (Tree)
import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (Positioned))

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Tree as Tree
import qualified Fana.Data.Tree.OfBase as Tree
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified Technical.TextTree.Data as Tt


type Text = Base.String

type ElemL e = Tt.Elem e
type ElemLT = ElemL Text
type ElemHR e = Tt.Elem e
type ElemHRT = ElemHR Text
type ElemHP e = Positioned e
type ElemHPT = ElemHP Text

parse :: Tree ElemLT -> Tree ElemHPT
parse = map Tt.elemValue >>> Tree.with_path_to_trunk >>> map (Bifunctor.first (map Tree.rootLabel) >>> uncurry Positioned)

layer :: Optic.Iso (Tree ElemLT) (Tree ElemLT) (Tree ElemHRT) (Tree ElemHPT)
layer = Optic.Iso id parse
