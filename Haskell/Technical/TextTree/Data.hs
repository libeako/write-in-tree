-- | TextTree data format - just a tree of texts [strings].
module Technical.TextTree.Data
(
	Text,
	Elem(..), Elem', 
	Tree, TreeT
)
where

import Data.Default.Class
import Fana.Prelude

import qualified Data.Tree as Tf
import qualified Prelude as Base


type Text = Base.String

data Elem v = Elem 
	{ elemId :: Maybe Text
		-- ^ . elements may be generated by this application, 
		-- then they do not have automatic identifier
	, elemValue :: v 
	} 
	deriving (Eq, Base.Show, Base.Functor, Base.Foldable, Base.Traversable)
type Elem' = Elem Text

instance Default v => Default (Elem v) where def = Elem def def

type Tree v = Tf.Tree (Elem v)
type TreeT = Tree Text
