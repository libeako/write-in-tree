-- | TextTree data format - just a tree of texts [strings].
module Technical.TextTree.Data
(
	Text,
	Elem(..), ElemT, 
	Tree, TreeT
)
where

import Data.Default.Class
import Fana.Prelude

import qualified Data.Tree as Tf
import qualified Prelude as Base


type Text = Base.String

data Elem v =
	Elem
	{ elemValue :: v
	}
	deriving (Eq, Base.Show, Base.Functor, Base.Foldable, Base.Traversable)
type ElemT = Elem Text

instance Default v => Default (Elem v) where def = Elem def

type Tree v = Tf.Tree (Elem v)
type TreeT = Tree Text
