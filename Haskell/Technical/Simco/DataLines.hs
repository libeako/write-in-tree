module Technical.Simco.DataLines
(
	MeaningfulCommon (..), MeaningfulNode (..), Node (..),
	process_Node,
	delete_not_active_from_forest,
	forest_to_map,
)
where

import Fana.Prelude
import Fana.Data.Tree.Uniform (Tree (..))
import Prelude (Bool, Eq (..), String)

import qualified Data.Foldable as Base
import qualified Data.Tree as Base
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.Tree.Uniform as FanaTree
import qualified Fana.Data.Tree.Discriminating as DiscrTree
import qualified Technical.ParsePropertyTree as PropTree


type Text = String
type Name = Text
type PropertyValue = Text

data MeaningfulCommon = MeaningfulCommon { mnIsActive :: Bool, mnName :: Name } deriving Eq
data MeaningfulNode = NodeCategory MeaningfulCommon | NodeProperty MeaningfulCommon PropertyValue
	deriving Eq
data Node = NodeMeaningful MeaningfulNode | NodeComment Text
	deriving Eq

process_Node :: (MeaningfulNode -> r) -> (Text -> r) -> Node -> r
process_Node on_meaningful on_comment = 
	\ case
		NodeMeaningful d -> on_meaningful d
		NodeComment d -> on_comment d


-- * filtering the active nodes :

type ActiveForest = [DiscrTree.Tree [] PropertyValue () Name]

delete_not_active_from_tree :: Base.Tree Node -> ActiveForest
delete_not_active_from_tree (Base.Node trunk children) =
	case trunk of
		NodeComment _ -> []
		NodeMeaningful meaningful -> 
			let
				answer :: 
					MeaningfulCommon -> 
					DiscrTree.Discrimination [] PropertyValue () 
						(Tree (DiscrTree.Discrimination [] PropertyValue ()) Name) -> 
					ActiveForest
				answer (MeaningfulCommon is_active name) node_specific_part = 
					if is_active then [FanaTree.assemble name node_specific_part] else []
				in
					case meaningful of
						NodeProperty common value -> answer common (DiscrTree.Leaf value)
						NodeCategory common -> answer common (DiscrTree.Joint () (delete_not_active_from_forest children))

delete_not_active_from_forest :: Base.Forest Node -> ActiveForest
delete_not_active_from_forest = map delete_not_active_from_tree >>> Base.concat


-- * mapping :

from_tree_to_key_value_at :: 
	[Name] -> DiscrTree.Tree [] PropertyValue () Name -> (Name, PropTree.InputMapElement)
from_tree_to_key_value_at path = 
	FanaTree.structure >>> 
	\ case 
		(name, node_specific) ->
			case node_specific of
				DiscrTree.Leaf property_value -> (name, PropTree.MeSingle property_value)
				DiscrTree.Joint _ children -> 
					(PropTree.MeComposite >>> Pair.after name) (forest_to_map_at (name : path) children)

forest_to_map_at :: [Name] -> ActiveForest -> PropTree.InputMap
forest_to_map_at path = map (from_tree_to_key_value_at path)

active_forest_to_map :: ActiveForest -> PropTree.InputMap
active_forest_to_map = forest_to_map_at []

forest_to_map :: Base.Forest Node -> PropTree.InputMap
forest_to_map = delete_not_active_from_forest >>> active_forest_to_map
