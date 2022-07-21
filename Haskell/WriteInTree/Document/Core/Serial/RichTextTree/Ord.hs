module WriteInTree.Document.Core.Serial.RichTextTree.Ord
(
	layer,
)
where

import Data.Tree (Tree)
import Fana.Prelude

import qualified Fana.Optic.Concrete.Categories.Iso as Optic


-- | labels the elements with their ordinal inside the list.
ord_list :: [e] -> [e]
ord_list = id

ord_trees :: [Tree e] -> [Tree e]
ord_trees = id

render :: Tree e -> Tree e
render = id

layer :: Optic.Iso' (Tree e) (Tree e)
layer = Optic.Iso id id
