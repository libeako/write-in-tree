-- | TextTree data format - just a tree of texts [strings].
module Technical.TextTree.Data
(
	Text, Elem(..),
)
where


import qualified Prelude as Base


type Text = Base.String

type Elem = Text
