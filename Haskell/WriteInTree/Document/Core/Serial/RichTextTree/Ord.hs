module WriteInTree.Document.Core.Serial.RichTextTree.Ord
(
	SimpleOrdinal, Ordinal, Ordered, 
	layer,
)
where

import Data.Tree (Tree)
import Fana.Prelude

import qualified Data.List as List
import qualified Data.Tree as Tree
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Optic.Concrete.Categories.Iso as Optic


type SimpleOrdinal = ()
type Ordinal = [SimpleOrdinal]

type Ordered e = (Ordinal, e)

-- | labels the elements with their ordinal inside the list.
ord_list :: [e] -> [Ordered e]
ord_list = List.zip (map (: []) (List.repeat ()))

ord_tree :: Ordered (Tree e) -> Tree (Ordered e)
ord_tree (ord, Tree.Node trunk children) = Tree.Node (ord, trunk) (ord_trees children)
-- | labels the elements with their ordinal inside the list of siblings.
ord_trees :: [Tree e] -> [Tree (Ordered e)]
ord_trees = ord_list >>> map ord_tree

render :: Tree (Ordered e) -> Tree e
render (Tree.Node (_, trunk) children) = Tree.Node trunk (map render (List.sortOn (Tree.rootLabel >>> fst) children))

layer :: Optic.Iso' (Tree e) (Tree (Ordered e))
layer = Optic.Iso render (Pair.after [()] >>> ord_tree)
