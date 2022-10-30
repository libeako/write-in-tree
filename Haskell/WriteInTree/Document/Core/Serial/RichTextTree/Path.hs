module WriteInTree.Document.Core.Serial.RichTextTree.Path
(
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

type ElemL = Tt.Elem
type ElemHP = Positioned Text

parse :: Tree ElemL -> Tree ElemHP
parse =
	id
	>>> Tree.with_path_to_trunk
	>>> map (Bifunctor.first (map Tree.rootLabel) >>> uncurry Positioned)

layer :: Optic.Iso (Tree ElemL) (Tree ElemL) (Tree Tt.Elem) (Tree ElemHP)
layer = Optic.Iso id parse
