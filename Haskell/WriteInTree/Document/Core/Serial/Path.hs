module WriteInTree.Document.Core.Serial.Path
(
	layer,
)
where

import Data.Tree (Tree, Forest)
import Fana.Prelude
import WriteInTree.Document.Core.Serial.Position (Positioned (Positioned))

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Tree as Tree
import qualified Fana.Data.Tree.OfBase as Tree
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base


type Text = Base.String

parse :: Tree Text -> Tree (Positioned Text)
parse =
	id
	>>> Tree.with_path_to_trunk
	>>> map (Bifunctor.first (map Tree.rootLabel) >>> uncurry Positioned)

layer :: Optic.Iso (Forest Text) (Forest Text) (Forest Text) (Forest (Positioned Text))
layer = Optic.Iso id (map parse)